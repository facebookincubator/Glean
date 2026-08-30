# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Implementation of the Haskell build rules.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:archive.bzl", "make_archive")
load(
    "@prelude//cxx:cxx.bzl",
    "get_auto_link_group_specs",
)
load(
    "@prelude//cxx:cxx_context.bzl",
    "get_cxx_toolchain_info",
)
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "CxxToolchainInfo",
    "LinkerType",
    "PicBehavior",
)
load("@prelude//cxx:groups.bzl", "get_dedupped_roots_from_groups")
load(
    "@prelude//cxx:link_groups.bzl",
    "BuildLinkGroupsContext",
    "LinkGroupContext",
    "collect_linkables",
    "create_link_groups",
    "find_relevant_roots",
    "get_filtered_labels_to_links_map",
    "get_filtered_links",
    "get_link_group_info",
    "get_link_group_preferred_linkage",
    "get_public_link_group_nodes",
    "get_transitive_deps_matching_labels",
    "is_link_group_shlib",
)
load(
    "@prelude//cxx:linker.bzl",
    "LINKERS",
    "get_rpath_origin",
    "get_shared_library_flags",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//haskell:compile.bzl",
    "CompileResultInfo",
    "compile",
)
load(
    "@prelude//haskell:haskell_haddock.bzl",
    "haskell_haddock_lib",
)
load(
    "@prelude//haskell:library_info.bzl",
    "HaskellLibraryInfo",
    "HaskellLibraryInfoTSet",
    "HaskellLibraryProvider",
)
load(
    "@prelude//haskell:link_info.bzl",
    "HaskellLinkInfo",
    "HaskellProfLinkInfo",
    "attr_link_style",
    "cxx_toolchain_link_style",
)
load(
    "@prelude//haskell:toolchain.bzl",
    "HaskellToolchainInfo",
)
load(
    "@prelude//haskell:util.bzl",
    "attr_deps",
    "attr_deps_haskell_link_infos_sans_template_deps",
    "attr_deps_merged_link_infos",
    "attr_deps_profiling_link_infos",
    "attr_deps_shared_library_infos",
    "get_artifact_suffix",
    "is_haskell_src",
    "output_extensions",
    "src_to_module_name",
    "srcs_to_pairs",
)
load(
    "@prelude//linking:link_groups.bzl",
    "gather_link_group_libs",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "Archive",
    "ArchiveContentsType",
    "ArchiveLinkable",
    "LibOutputStyle",
    "LinkArgs",
    "LinkInfo",
    "LinkInfos",
    "LinkOrdering",
    "LinkStyle",
    "LinkedObject",
    "MergedLinkInfo",
    "SharedLibLinkable",
    "create_merged_link_info",
    "default_output_style_for_link_strategy",
    "get_lib_output_style",
    "get_link_args_for_strategy",
    "get_output_styles_for_linkage",
    "legacy_output_style_to_link_style",
    "to_link_strategy",
    "unpack_link_args",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableGraph",
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
    "reduce_linkable_graph",
)
load(
    "@prelude//linking:linkables.bzl",
    "linkables",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "create_shared_libraries",
    "create_shlib_symlink_tree",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load("@prelude//linking:types.bzl", "Linkage")
load(
    "@prelude//python:python.bzl",
    "PythonLibraryInfo",
)
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:utils.bzl", "filter_and_map_idx", "flatten")

HaskellIndexingTSet = transitive_set()

# A list of hie dirs
HaskellIndexInfo = provider(
    fields = {
        "info": provider_field(typing.Any, default = None),  # dict[LinkStyle, HaskellIndexingTset]
    },
)

# This conversion is non-standard, see TODO about link style below
def _to_lib_output_style(link_style: LinkStyle) -> LibOutputStyle:
    return default_output_style_for_link_strategy(to_link_strategy(link_style))

def _attr_preferred_linkage(ctx: AnalysisContext) -> Linkage:
    preferred_linkage = ctx.attrs.preferred_linkage

    # force_static is deprecated, but it has precedence over preferred_linkage
    if getattr(ctx.attrs, "force_static", False):
        preferred_linkage = "static"

    return Linkage(preferred_linkage)

# --

def _get_haskell_prebuilt_libs(ctx, link_style: LinkStyle, enable_profiling: bool) -> list[Artifact]:
    if link_style == LinkStyle("shared"):
        if enable_profiling:
            # Profiling doesn't support shared libraries
            return []

        return ctx.attrs.shared_libs.values()
    elif link_style == LinkStyle("static"):
        if enable_profiling:
            return ctx.attrs.profiled_static_libs
        return ctx.attrs.static_libs
    elif link_style == LinkStyle("static_pic"):
        if enable_profiling:
            return ctx.attrs.pic_profiled_static_libs
        return ctx.attrs.pic_static_libs
    else:
        fail("Unexpected LinkStyle: " + link_style.value)

def haskell_prebuilt_library_impl(ctx: AnalysisContext) -> list[Provider]:
    # MergedLinkInfo for both with and without profiling
    native_infos = []
    prof_native_infos = []

    haskell_infos = []
    shared_library_infos = []
    for dep in ctx.attrs.deps:
        used = False
        if HaskellLinkInfo in dep:
            used = True
            haskell_infos.append(dep[HaskellLinkInfo])
        li = dep.get(MergedLinkInfo)
        if li != None:
            used = True
            native_infos.append(li)
            if HaskellLinkInfo not in dep:
                prof_native_infos.append(li)
        if HaskellProfLinkInfo in dep:
            prof_native_infos.append(dep[HaskellProfLinkInfo].prof_infos)
        if SharedLibraryInfo in dep:
            used = True
            shared_library_infos.append(dep[SharedLibraryInfo])
        if PythonLibraryInfo in dep:
            used = True
        if not used:
            fail("Unexpected link info encountered")

    hlibinfos = {}
    prof_hlibinfos = {}
    hlinkinfos = {}
    prof_hlinkinfos = {}
    link_infos = {}
    prof_link_infos = {}
    for link_style in LinkStyle:
        libs = _get_haskell_prebuilt_libs(ctx, link_style, False)
        prof_libs = _get_haskell_prebuilt_libs(ctx, link_style, True)

        hlibinfo = HaskellLibraryInfo(
            name = ctx.attrs.name,
            db = ctx.attrs.db,
            import_dirs = {},
            stub_dirs = [],
            id = ctx.attrs.id,
            libs = libs,
            version = ctx.attrs.version,
            is_prebuilt = True,
            profiling_enabled = False,
        )
        prof_hlibinfo = HaskellLibraryInfo(
            name = ctx.attrs.name,
            db = ctx.attrs.db,
            import_dirs = {},
            stub_dirs = [],
            id = ctx.attrs.id,
            libs = prof_libs,
            version = ctx.attrs.version,
            is_prebuilt = True,
            profiling_enabled = True,
        )

        def archive_linkable(lib):
            return ArchiveLinkable(
                archive = Archive(artifact = lib),
                linker_type = LinkerType("gnu"),
            )

        def shared_linkable(lib):
            return SharedLibLinkable(
                lib = lib,
            )

        linkables = [(shared_linkable if link_style == LinkStyle("shared") else archive_linkable)(lib) for lib in libs]
        prof_linkables = [(shared_linkable if link_style == LinkStyle("shared") else archive_linkable)(lib) for lib in prof_libs]

        hlibinfos[link_style] = hlibinfo
        hlinkinfos[link_style] = ctx.actions.tset(
            HaskellLibraryInfoTSet,
            value = hlibinfo,
            children = [lib.info[link_style] for lib in haskell_infos],
        )
        prof_hlibinfos[link_style] = prof_hlibinfo
        prof_hlinkinfos[link_style] = ctx.actions.tset(
            HaskellLibraryInfoTSet,
            value = prof_hlibinfo,
            children = [lib.prof_info[link_style] for lib in haskell_infos],
        )
        link_infos[link_style] = LinkInfos(
            default = LinkInfo(
                pre_flags = ctx.attrs.exported_linker_flags,
                post_flags = ctx.attrs.exported_post_linker_flags,
                linkables = linkables,
            ),
        )
        prof_link_infos[link_style] = LinkInfos(
            default = LinkInfo(
                pre_flags = ctx.attrs.exported_linker_flags,
                post_flags = ctx.attrs.exported_post_linker_flags,
                linkables = prof_linkables,
            ),
        )

    haskell_link_infos = HaskellLinkInfo(
        info = hlinkinfos,
        prof_info = prof_hlinkinfos,
    )
    haskell_lib_provider = HaskellLibraryProvider(
        lib = hlibinfos,
        prof_lib = prof_hlibinfos,
    )

    # The link info that will be used when this library is a dependency of a non-Haskell
    # target (e.g. a cxx_library()). We need to pick the profiling libs if we're in
    # profiling mode.
    default_link_infos = prof_link_infos if ctx.attrs.enable_profiling else link_infos
    default_native_infos = prof_native_infos if ctx.attrs.enable_profiling else native_infos
    merged_link_info = create_merged_link_info(
        ctx,
        # We don't have access to a CxxToolchain here (yet).
        # Give that it's already built, this doesn't mean much, use a sane default.
        pic_behavior = PicBehavior("supported"),
        link_infos = {_to_lib_output_style(s): v for s, v in default_link_infos.items()},
        exported_deps = default_native_infos,
    )

    prof_merged_link_info = create_merged_link_info(
        ctx,
        # We don't have access to a CxxToolchain here (yet).
        # Give that it's already built, this doesn't mean much, use a sane default.
        pic_behavior = PicBehavior("supported"),
        link_infos = {_to_lib_output_style(s): v for s, v in prof_link_infos.items()},
        exported_deps = prof_native_infos,
    )

    solibs = {}
    for soname, lib in ctx.attrs.shared_libs.items():
        solibs[soname] = LinkedObject(output = lib, unstripped_output = lib)
    shared_libs = create_shared_libraries(ctx, solibs)

    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            linkable_node = create_linkable_node(
                ctx = ctx,
                exported_deps = ctx.attrs.deps,
                link_infos = {_to_lib_output_style(s): v for s, v in link_infos.items()},
                shared_libs = shared_libs,
                default_soname = None,
            ),
        ),
        deps = ctx.attrs.deps,
    )

    inherited_pp_info = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    own_pp_info = CPreprocessor(
        args = CPreprocessorArgs(args = flatten([["-isystem", d] for d in ctx.attrs.cxx_header_dirs])),
    )

    return [
        DefaultInfo(),
        haskell_lib_provider,
        cxx_merge_cpreprocessors(ctx.actions, [own_pp_info], inherited_pp_info),
        merge_shared_libraries(
            ctx.actions,
            shared_libs,
            shared_library_infos,
        ),
        merge_link_group_lib_info(deps = ctx.attrs.deps),
        haskell_link_infos,
        merged_link_info,
        HaskellProfLinkInfo(
            prof_infos = prof_merged_link_info,
        ),
        linkable_graph,
    ]

def _srcs_to_objfiles(ctx: AnalysisContext, odir: Artifact, osuf: str) -> list[Artifact]:
    objfiles = []
    for src, _ in srcs_to_pairs(ctx.attrs.srcs):
        # Don't link boot sources, as they're only meant to be used for compiling.
        if is_haskell_src(src):
            objfiles.append(odir.project(paths.replace_extension(src, "." + osuf)))
    return objfiles

_REGISTER_PACKAGE = """\
set -eu
GHC_PKG=$1
DB=$2
PKGCONF=$3
ALWAYS_USE_CACHE=$4
"$GHC_PKG" init "$DB"
"$GHC_PKG" register --package-conf "$DB" --no-expand-pkgroot $ALWAYS_USE_CACHE "$PKGCONF"
"""

# Create a package
#
# The way we use packages is a bit strange. We're not using them
# at link time at all: all the linking info is in the
# HaskellLibraryInfo and we construct linker command lines
# manually. Packages are used for:
#
#  - finding .hi files at compile time
#
#  - symbol namespacing (so that modules with the same name in
#    different libraries don't clash).
#
#  - controlling module visibility: only dependencies that are
#    directly declared as dependencies may be used
#
#  - Template Haskell: the compiler needs to load libraries itself
#    at compile time, so it uses the package specs to find out
#    which libraries and where.
def _make_package(
    ctx: AnalysisContext,
    link_style: LinkStyle,
    pkgname: str,
    libname: str,
    hlis: list[HaskellLibraryInfo],
    hi: dict[bool, Artifact],
    lib: dict[bool, Artifact],
    enable_profiling: bool,
    # Overrides which link_style's artifact-suffix convention `hi`'s
    # directory name is computed from - only needed when `hi` wasn't built
    # for `link_style` itself (see _build_haskell_lib's shared-via-
    # dynamic-too derivation, where a "shared" package's interfaces
    # actually live in the *static* compile's "hi-static", written there by
    # -dynamic-too alongside the plain .hi this "shared" package reads).
    # Defaults to `link_style`, matching every other, non-derived caller.
    hi_link_style: [LinkStyle, None] = None,
    # This same target's own "shared" variant (built alongside any other
    # link style whenever Linkage allows it - see haskell_library_impl).
    # Referenced here (for non-shared link styles) via `dynamic-library-dirs:`
    # so that GHC's internal interpreter can load this package the dynamic
    # way for a Template Haskell splice, even when this specific conf is
    # for a statically-linked consumer. GHC's `--make` driver dual-compiles
    # *every* module in a compilation unit the moment any one of them uses
    # TemplateHaskell (confirmed empirically - it needs a dynamic way for
    # every import reached that way, not just modules a splice actually
    # touches), so every package needs to be discoverable this way, not
    # just ones that use TH themselves.
    own_shared_lib: [Artifact, None] = None,
) -> Artifact:
    artifact_suffix = get_artifact_suffix(link_style, enable_profiling)
    hi_link_style = hi_link_style if hi_link_style != None else link_style

    # Don't expose boot sources, as they're only meant to be used for compiling.
    modules = [src_to_module_name(x) for x, _ in srcs_to_pairs(ctx.attrs.srcs) if is_haskell_src(x)]

    if enable_profiling:
        # Add the `-p` suffix otherwise ghc will look for objects
        # following this logic (https://fburl.com/code/3gmobm5x) and will fail.
        libname += "_p"

    def mk_artifact_dir(dir_prefix: str, profiled: bool, for_style: LinkStyle = link_style) -> str:
        art_suff = get_artifact_suffix(for_style, profiled)
        return '"${pkgroot}/' + dir_prefix + "-" + art_suff + '"'

    import_dirs = [mk_artifact_dir("hi", profiled, hi_link_style) for profiled in hi.keys()]
    library_dirs = [mk_artifact_dir("lib", profiled) for profiled in hi.keys()]

    conf = [
        "name: " + pkgname,
        "version: 1.0.0",
        "id: " + pkgname,
        "key: " + pkgname,
        "exposed: False",
        "exposed-modules: " + ", ".join(modules),
        "import-dirs:" + ", ".join(import_dirs),
        "library-dirs:" + ", ".join(library_dirs),
        "extra-libraries: " + libname,
        "depends: " + ", ".join([lib.id for lib in hlis]),
    ]
    if own_shared_lib != None:
        conf.append('dynamic-library-dirs:"${pkgroot}/lib-shared"')
    pkg_conf = ctx.actions.write("pkg-" + artifact_suffix + ".conf", conf, has_content_based_path = False)

    db = ctx.actions.declare_output("db-" + artifact_suffix, has_content_based_path = False)

    # While the list of hlis is unique, there may be multiple packages in the same db.
    # Cutting down the GHC_PACKAGE_PATH significantly speeds up GHC.
    db_deps = {x.db: None for x in hlis}.keys()

    # So that ghc-pkg can find the DBs for the dependencies. We might
    # be able to use flags for this instead, but this works.
    ghc_package_path = cmd_args(
        db_deps,
        delimiter = ":",
    )

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    # --always-use-cache is a custom option to ghc-pkg that tells it to ignore the
    # modification time on the package cache and use it anyway. This is useful in
    # RE where file modification times can't be relied upon; without this option
    # ghc-pkg will fall back to reading all the package configs which is much
    # slower.
    if haskell_toolchain.support_always_use_cache:
        use_cache_arg = "--always-use-cache"
    else:
        use_cache_arg = ""

    ctx.actions.run(
        cmd_args(
            [
                "sh",
                "-c",
                _REGISTER_PACKAGE,
                "",
                haskell_toolchain.packager,
                db.as_output(),
                pkg_conf,
                use_cache_arg,
            ],
            # needs hi, because ghc-pkg checks that the .hi files exist
            hidden = hi.values() + lib.values() + ([own_shared_lib] if own_shared_lib != None else []),
        ),
        category = "haskell_package_" + artifact_suffix.replace("-", "_"),
        env = {"GHC_PACKAGE_PATH": ghc_package_path} if db_deps else {},
    )

    return db

HaskellLibBuildOutput = record(
    hlib = HaskellLibraryInfo,
    solibs = dict[str, LinkedObject],
    link_infos = LinkInfos,
    compiled = CompileResultInfo,
    libs = list[Artifact],
)

def _get_haskell_shared_library_name_linker_flags(linker_type: LinkerType, soname: str) -> list[str]:
    if linker_type == LinkerType("gnu"):
        return ["-Wl,-soname,{}".format(soname)]
    elif linker_type == LinkerType("darwin"):
        # Passing `-install_name @rpath/...` or
        # `-Xlinker -install_name -Xlinker @rpath/...` instead causes
        # ghc-9.6.3: panic! (the 'impossible' happened)
        return ["-Wl,-install_name,@rpath/{}".format(soname)]
    else:
        fail("Unknown linker type '{}'.".format(linker_type))

# A merged symlink tree of every native (cxx_library) shared library reached
# through this target's deps, named by SONAME - the same shape as the tree
# haskell_binary_impl builds for its own final-link rpath (see
# create_shlib_symlink_tree's other call site below), but built here so it
# can be handed to compile() as well: Template Haskell splices dlopen their
# dependencies' shared objects *during* compilation, not just at final link,
# so the compile action's sandbox needs the same search path. Returns None
# when there's nothing to add, so callers can skip the LD_LIBRARY_PATH
# plumbing entirely for targets with no shared native deps. Called once per
# target (not once per link_style/profiling variant compiled) since it maps
# to a single declared symlink-tree output.
def _native_shared_libs_dir(actions: AnalysisActions, name: str, shared_library_infos: list[SharedLibraryInfo]) -> [Artifact, None]:
    if not shared_library_infos:
        return None
    shlib_info = merge_shared_libraries(actions, deps = shared_library_infos)
    sos = traverse_shared_library_info(shlib_info, transformation_provider = None)
    if not sos:
        return None
    return create_shlib_symlink_tree(
        actions = actions,
        out = "__{}__th_shared_libs_symlink_tree".format(name),
        shared_libs = sos,
    )

# Links a Haskell shared library from a set of already-compiled object
# files. Factored out of _build_haskell_lib's own "shared" branch so the
# same link command can be reused for a shared library *derived* from a
# static compile's `-dynamic-too` byproduct (see build_shared_too below),
# not just one built from its own independent compile.
def _link_haskell_shared_lib(
        ctx,
        haskell_toolchain,
        linker_info,
        nlis: list[MergedLinkInfo],
        libfile: str,
        lib_short_path: str,
        objfiles,
        hidden,
        category: str) -> (Artifact, LinkedObject, LinkInfos):
    lib = ctx.actions.declare_output(lib_short_path, has_content_based_path = False)
    link = cmd_args(
        [haskell_toolchain.linker]
        + [haskell_toolchain.linker_flags]
        + [ctx.attrs.linker_flags]
        + ["-o", lib.as_output()]
        + [
            "-package-env=-",
            get_shared_library_flags(linker_info.type),
            "-dynamic",
            cmd_args(
                _get_haskell_shared_library_name_linker_flags(linker_info.type, libfile),
                prepend = "-optl",
            ),
        ]
        + [objfiles],
        hidden = hidden,
    )

    infos = get_link_args_for_strategy(
        ctx.actions,
        ctx.label,
        linker_info,
        nlis,
        to_link_strategy(LinkStyle("shared")),
        prefer_stripped = False,
        transformation_spec_context = None,
    )
    link.add(cmd_args(unpack_link_args(infos), prepend = "-optl"))
    ctx.actions.run(
        link,
        category = category,
    )

    return (
        lib,
        LinkedObject(output = lib, unstripped_output = lib),
        LinkInfos(default = LinkInfo(linkables = [SharedLibLinkable(lib = lib)])),
    )

def _build_haskell_lib(
    ctx,
    libname: str,
    pkgname: str,
    hlis: list[HaskellLinkInfo],  # haskell link infos from all deps
    nlis: list[MergedLinkInfo],  # native link infos from all deps
    link_style: LinkStyle,
    enable_profiling: bool,
    # Precomputed once per target (not per link_style/profiling variant - see
    # call site) since it maps to a single declared symlink-tree output.
    native_shared_libs_dir: [Artifact, None],
    # The non-profiling artifacts are also needed to build the package for
    # profiling, so it should be passed when `enable_profiling` is True.
    non_profiling_hlib: [HaskellLibBuildOutput, None] = None,
    # When True (only meaningful when link_style != "shared", and never
    # combined with enable_profiling - shared doesn't support profiling),
    # additionally derive a "shared" sibling from this same compile's
    # `-dynamic-too` byproduct (its .dyn_o/.dyn_hi are guaranteed
    # consistent with this compile's own .o/.hi, since they come from the
    # same compile pass), instead of haskell_library_impl building "shared"
    # independently. Returned as the second element of the result tuple
    # (None when not requested).
    build_shared_too: bool = False,
) -> (HaskellLibBuildOutput, [HaskellLibBuildOutput, None]):
    linker_info = ctx.attrs._cxx_toolchain[CxxToolchainInfo].linker_info

    # Link the objects into a library
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    osuf, _hisuf = output_extensions(link_style, enable_profiling)

    dynamic_too = build_shared_too and link_style != LinkStyle("shared")

    # Compile the sources
    compiled = compile(
        ctx,
        link_style,
        enable_profiling = enable_profiling,
        pkgname = pkgname,
        native_shared_libs_dir = native_shared_libs_dir,
        dynamic_too = dynamic_too,
    )
    solibs = {}
    artifact_suffix = get_artifact_suffix(link_style, enable_profiling)

    libstem = libname
    if link_style == LinkStyle("static_pic"):
        libstem += "_pic"

    dynamic_lib_suffix = "." + LINKERS[linker_info.type].default_shared_library_extension
    static_lib_suffix = "_p.a" if enable_profiling else ".a"
    libfile = "lib" + libstem + (dynamic_lib_suffix if link_style == LinkStyle("shared") else static_lib_suffix)

    lib_short_path = paths.join("lib-{}".format(artifact_suffix), libfile)

    linfos = [x.prof_info if enable_profiling else x.info for x in hlis]

    # only gather direct dependencies
    uniq_infos = [x[link_style].value for x in linfos]

    objfiles = _srcs_to_objfiles(ctx, compiled.objects, osuf)

    if link_style == LinkStyle("shared"):
        lib, solib, link_infos = _link_haskell_shared_lib(
            ctx,
            haskell_toolchain,
            linker_info,
            nlis,
            libfile,
            lib_short_path,
            objfiles,
            compiled.stubs,
            "haskell_link" + artifact_suffix.replace("-", "_"),
        )
        solibs[libfile] = solib
        libs = [lib]

    else:  # static flavours
        # TODO: avoid making an archive for a single object, like cxx does
        # (but would that work with Template Haskell?)
        # TODO: Opt haskell actions into content based paths.
        archive = make_archive(ctx, lib_short_path, objfiles, force_disable_content_based_path = True)
        lib = archive.artifact
        libs = [lib] + (archive.external_objects if archive.archive_contents_type == ArchiveContentsType("thin") else [])
        link_infos = LinkInfos(
            default = LinkInfo(
                linkables = [
                    ArchiveLinkable(
                        archive = archive,
                        linker_type = linker_info.type,
                        link_whole = ctx.attrs.link_whole,
                    ),
                ],
            ),
        )

    if enable_profiling and link_style != LinkStyle("shared"):
        if not non_profiling_hlib:
            fail("Non-profiling HaskellLibBuildOutput wasn't provided when building profiling lib")

        import_artifacts = {
            True: compiled.hi,
            False: non_profiling_hlib.compiled.hi,
        }
        library_artifacts = {
            True: lib,
            False: non_profiling_hlib.libs[0],
        }
        all_libs = libs + non_profiling_hlib.libs
        stub_dirs = [compiled.stubs] + [non_profiling_hlib.compiled.stubs]
    else:
        import_artifacts = {
            False: compiled.hi,
        }
        library_artifacts = {
            False: lib,
        }
        all_libs = libs
        stub_dirs = [compiled.stubs]

    # Derive the shared library from this compile's own -dynamic-too
    # byproduct - its .dyn_o objects and .dyn_hi interfaces - rather than
    # compiling "shared" independently (see build_shared_too's docstring
    # above for why that's safe and this is worthwhile). Done *before* this
    # package's own _make_package call below, since that call needs the
    # resulting artifact too (own_shared_lib, for `dynamic-library-dirs:` -
    # this package's own conf is what *other* packages doing Template
    # Haskell will actually load).
    own_shared_lib = None
    shared_output = None
    if dynamic_too:
        shared_libfile = "lib" + libname + dynamic_lib_suffix
        shared_lib_short_path = paths.join("lib-shared", shared_libfile)
        dyn_objfiles = _srcs_to_objfiles(ctx, compiled.objects, "dyn_o")
        shared_lib, shared_solib, shared_link_infos = _link_haskell_shared_lib(
            ctx,
            haskell_toolchain,
            linker_info,
            nlis,
            shared_libfile,
            shared_lib_short_path,
            dyn_objfiles,
            compiled.stubs,
            "haskell_link_shared_derived",
        )
        own_shared_lib = shared_lib

        # GHC's -dynamic-too guarantees the .dyn_hi it writes alongside
        # this compile's own .hi is fully consistent with it (same compile
        # pass, same frontend work) - so a "shared" consumer can safely use
        # this same `compiled.hi` directory too. It contains both .hi and
        # .dyn_hi; a plain `-hisuf hi` reader (which is what a "shared"
        # package's own db expects) only ever looks for the former.
        shared_db = _make_package(
            ctx,
            LinkStyle("shared"),
            pkgname,
            libname,
            uniq_infos,
            {False: compiled.hi},
            {False: shared_lib},
            enable_profiling = False,
            hi_link_style = link_style,
        )
        shared_hlib = HaskellLibraryInfo(
            name = pkgname,
            db = shared_db,
            id = pkgname,
            import_dirs = {False: compiled.hi},
            stub_dirs = [compiled.stubs],
            libs = [shared_lib],
            version = "1.0.0",
            is_prebuilt = False,
            profiling_enabled = False,
        )
        shared_output = HaskellLibBuildOutput(
            hlib = shared_hlib,
            solibs = {shared_libfile: shared_solib},
            link_infos = shared_link_infos,
            compiled = compiled,
            libs = [shared_lib],
        )

    db = _make_package(
        ctx,
        link_style,
        pkgname,
        libstem,
        uniq_infos,
        import_artifacts,
        library_artifacts,
        enable_profiling = enable_profiling,
        own_shared_lib = own_shared_lib,
    )

    hlib = HaskellLibraryInfo(
        name = pkgname,
        db = db,
        id = pkgname,
        import_dirs = import_artifacts,
        stub_dirs = stub_dirs,
        libs = all_libs,
        version = "1.0.0",
        is_prebuilt = False,
        profiling_enabled = enable_profiling,
    )

    main_output = HaskellLibBuildOutput(
        hlib = hlib,
        solibs = solibs,
        link_infos = link_infos,
        compiled = compiled,
        libs = libs,
    )

    return (main_output, shared_output)

def haskell_library_impl(ctx: AnalysisContext) -> list[Provider]:
    preferred_linkage = _attr_preferred_linkage(ctx)
    if ctx.attrs.enable_profiling and preferred_linkage == Linkage("any"):
        preferred_linkage = Linkage("static")

    # Get haskell and native link infos from all deps
    hlis = attr_deps_haskell_link_infos_sans_template_deps(ctx)
    nlis = attr_deps_merged_link_infos(ctx)
    prof_nlis = attr_deps_profiling_link_infos(ctx)
    shared_library_infos = attr_deps_shared_library_infos(ctx)

    solibs = {}
    link_infos = {}
    prof_link_infos = {}
    hlib_infos = {}
    hlink_infos = {}
    prof_hlib_infos = {}
    prof_hlink_infos = {}
    indexing_tsets = {}
    sub_targets = {}

    libname = repr(ctx.label.path).replace("//", "_").replace("/", "_") + "_" + ctx.label.name
    pkgname = libname.replace("_", "-")

    native_shared_libs_dir = _native_shared_libs_dir(ctx.actions, libname, shared_library_infos)

    link_styles = [legacy_output_style_to_link_style(o) for o in get_output_styles_for_linkage(preferred_linkage)]

    # See build_shared_too on _build_haskell_lib: when set (opt mode - see
    # buck2/haskell.bzl), and this Linkage wants both a static and a shared
    # output style, build them together from one -dynamic-too compile
    # instead of two independent ones. Only meaningful when both exist.
    build_shared_too = (
        getattr(ctx.attrs, "dynamic_too", False) and
        LinkStyle("static") in link_styles and
        LinkStyle("shared") in link_styles
    )

    # Records one built variant's outputs into this function's accumulating
    # dicts - factored out so the "shared" variant derived alongside
    # "static" (see below) is recorded the same way a normally-built one
    # would be, without duplicating this bookkeeping.
    def record(link_style: LinkStyle, enable_profiling: bool, hlib_build_out: HaskellLibBuildOutput):
        hlib = hlib_build_out.hlib
        solibs.update(hlib_build_out.solibs)
        compiled = hlib_build_out.compiled
        libs = hlib_build_out.libs

        if enable_profiling:
            prof_hlib_infos[link_style] = hlib
            prof_hlink_infos[link_style] = ctx.actions.tset(
                HaskellLibraryInfoTSet,
                value = hlib,
                children = [li.prof_info[link_style] for li in hlis],
            )
            prof_link_infos[link_style] = hlib_build_out.link_infos
        else:
            hlib_infos[link_style] = hlib
            hlink_infos[link_style] = ctx.actions.tset(
                HaskellLibraryInfoTSet,
                value = hlib,
                children = [li.info[link_style] for li in hlis],
            )
            link_infos[link_style] = hlib_build_out.link_infos

        # Build the indices and create subtargets only once, with profiling
        # enabled or disabled based on what was set in the library's
        # target.
        if ctx.attrs.enable_profiling == enable_profiling:
            if compiled.producing_indices:
                tset = derive_indexing_tset(
                    ctx.actions,
                    link_style,
                    compiled.hi,
                    attr_deps(ctx),
                )
                indexing_tsets[link_style] = tset

            sub_targets[link_style.value.replace("_", "-")] = [
                DefaultInfo(
                    default_outputs = libs,
                )
            ]

    # The non-profiling library is also needed to build the package with
    # profiling enabled, so we need to keep track of it for each link style.
    non_profiling_hlib = {}
    for enable_profiling in [False, True]:
        for output_style in get_output_styles_for_linkage(preferred_linkage):
            link_style = legacy_output_style_to_link_style(output_style)
            if link_style == LinkStyle("shared") and enable_profiling:
                # Profiling isn't support with dynamic linking
                continue

            if build_shared_too and link_style == LinkStyle("shared"):
                # Derived below, alongside "static" - see the
                # build_shared_too branch of this same iteration.
                continue

            hlib_build_out, derived_shared = _build_haskell_lib(
                ctx,
                libname,
                pkgname,
                hlis = hlis,
                nlis = nlis,
                link_style = link_style,
                enable_profiling = enable_profiling,
                native_shared_libs_dir = native_shared_libs_dir,
                non_profiling_hlib = non_profiling_hlib.get(link_style),
                build_shared_too = (build_shared_too and link_style == LinkStyle("static") and not enable_profiling),
            )
            if not enable_profiling:
                non_profiling_hlib[link_style] = hlib_build_out

            record(link_style, enable_profiling, hlib_build_out)

            if derived_shared != None:
                non_profiling_hlib[LinkStyle("shared")] = derived_shared
                record(LinkStyle("shared"), False, derived_shared)

    pic_behavior = ctx.attrs._cxx_toolchain[CxxToolchainInfo].pic_behavior
    link_style = cxx_toolchain_link_style(ctx)
    output_style = get_lib_output_style(
        to_link_strategy(link_style),
        preferred_linkage,
        pic_behavior,
    )
    shared_libs = create_shared_libraries(ctx, solibs)

    # TODO(cjhopman): this haskell implementation does not consistently handle LibOutputStyle
    # and LinkStrategy as expected and it's hard to tell what the intent of the existing code is
    # and so we currently just preserve its existing use of the legacy LinkStyle type and just
    # naively convert it at the boundaries of other code. This needs to be cleaned up by someone
    # who understands the intent of the code here.
    actual_link_style = legacy_output_style_to_link_style(output_style)

    if preferred_linkage != Linkage("static"):
        # Profiling isn't support with dynamic linking, but `prof_link_infos`
        # needs entries for all link styles.
        # We only need to set the shared link_style in both `prof_link_infos`
        # and `link_infos` if the target doesn't force static linking.
        prof_link_infos[LinkStyle("shared")] = link_infos[LinkStyle("shared")]

    default_link_infos = prof_link_infos if ctx.attrs.enable_profiling else link_infos
    default_native_infos = prof_nlis if ctx.attrs.enable_profiling else nlis
    merged_link_info = create_merged_link_info(
        ctx,
        pic_behavior = pic_behavior,
        link_infos = {_to_lib_output_style(s): v for s, v in default_link_infos.items()},
        preferred_linkage = preferred_linkage,
        exported_deps = default_native_infos,
    )

    prof_merged_link_info = create_merged_link_info(
        ctx,
        pic_behavior = pic_behavior,
        link_infos = {_to_lib_output_style(s): v for s, v in prof_link_infos.items()},
        preferred_linkage = preferred_linkage,
        exported_deps = prof_nlis,
    )

    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            linkable_node = create_linkable_node(
                ctx = ctx,
                preferred_linkage = preferred_linkage,
                exported_deps = ctx.attrs.deps,
                link_infos = {_to_lib_output_style(s): v for s, v in link_infos.items()},
                shared_libs = shared_libs,
                # TODO(cjhopman): this should be set to non-None
                default_soname = None,
            ),
        ),
        deps = ctx.attrs.deps,
    )

    default_output = hlib_infos[actual_link_style].libs

    inherited_pp_info = cxx_inherited_preprocessor_infos(attr_deps(ctx))

    # We would like to expose the generated _stub.h headers to C++
    # compilations, but it's hard to do that without overbuilding. Which
    # link_style should we pick below? If we pick a different link_style from
    # the one being used by the root rule, we'll end up building all the
    # Haskell libraries multiple times.
    #
    #    pp = [CPreprocessor(
    #        args =
    #            flatten([["-isystem", dir] for dir in hlib_infos[actual_link_style].stub_dirs]),
    #    )]
    pp = []

    providers = [
        DefaultInfo(
            default_outputs = default_output,
            sub_targets = sub_targets,
        ),
        HaskellLibraryProvider(
            lib = hlib_infos,
            prof_lib = prof_hlib_infos,
        ),
        HaskellLinkInfo(
            info = hlink_infos,
            prof_info = prof_hlink_infos,
        ),
        merged_link_info,
        HaskellProfLinkInfo(
            prof_infos = prof_merged_link_info,
        ),
        linkable_graph,
        cxx_merge_cpreprocessors(ctx.actions, pp, inherited_pp_info),
        merge_shared_libraries(
            ctx.actions,
            shared_libs,
            shared_library_infos,
        ),
        haskell_haddock_lib(ctx, pkgname),
    ]

    if indexing_tsets:
        providers.append(HaskellIndexInfo(info = indexing_tsets))

    # TODO(cjhopman): This code is for templ_vars is duplicated from cxx_library
    templ_vars = {}

    # Add in ldflag macros.
    for link_style in (LinkStyle("static"), LinkStyle("static_pic")):
        name = "ldflags-" + link_style.value.replace("_", "-")
        args = cmd_args()
        linker_info = ctx.attrs._cxx_toolchain[CxxToolchainInfo].linker_info
        args.add(linker_info.linker_flags)
        args.add(
            unpack_link_args(
                get_link_args_for_strategy(
                    ctx.actions,
                    ctx.label,
                    linker_info,
                    [merged_link_info],
                    to_link_strategy(link_style),
                    prefer_stripped = False,
                    transformation_spec_context = None,
                ),
            )
        )
        templ_vars[name] = args

    # TODO(T110378127): To implement `$(ldflags-shared ...)` properly, we'd need
    # to setup a symink tree rule for all transitive shared libs.  Since this
    # currently would be pretty costly (O(N^2)?), and since it's not that
    # commonly used anyway, just use `static-pic` instead.  Longer-term, once
    # v1 is gone, macros that use `$(ldflags-shared ...)` (e.g. Haskell's
    # hsc2hs) can move to a v2 rules-based API to avoid needing this macro.
    templ_vars["ldflags-shared"] = templ_vars["ldflags-static-pic"]

    providers.append(TemplatePlaceholderInfo(keyed_variables = templ_vars))

    providers.append(merge_link_group_lib_info(deps = attr_deps(ctx)))

    return providers

# TODO(cjhopman): should this be LibOutputType or LinkStrategy?
def derive_indexing_tset(actions: AnalysisActions, link_style: LinkStyle, value: Artifact | None, children: list[Dependency]) -> HaskellIndexingTSet:
    index_children = []
    for dep in children:
        li = dep.get(HaskellIndexInfo)
        if li:
            if link_style in li.info:
                index_children.append(li.info[link_style])

    return actions.tset(
        HaskellIndexingTSet,
        value = value,
        children = index_children,
    )

def haskell_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    enable_profiling = ctx.attrs.enable_profiling

    # Decide what kind of linking we're doing

    link_style = attr_link_style(ctx)

    # Link Groups
    link_group_info = get_link_group_info(
        ctx,
        filter_and_map_idx(LinkableGraph, attr_deps(ctx)),
        to_link_strategy(link_style),
    )

    # Profiling doesn't support shared libraries
    if enable_profiling and link_style == LinkStyle("shared"):
        link_style = LinkStyle("static")

    compiled = compile(
        ctx,
        link_style,
        enable_profiling = enable_profiling,
        native_shared_libs_dir = _native_shared_libs_dir(ctx.actions, ctx.attrs.name, attr_deps_shared_library_infos(ctx)),
    )

    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    output = ctx.actions.declare_output(ctx.attrs.name, has_content_based_path = False)
    link = cmd_args(
        [haskell_toolchain.compiler] + ["-o", output.as_output()] + [haskell_toolchain.linker_flags] + [ctx.attrs.linker_flags],
        hidden = compiled.stubs,
    )

    link_args = cmd_args("-package-env=-")

    osuf, _hisuf = output_extensions(link_style, enable_profiling)

    objfiles = _srcs_to_objfiles(ctx, compiled.objects, osuf)
    link_args.add(objfiles)

    indexing_tsets = {}
    if compiled.producing_indices:
        tset = derive_indexing_tset(ctx.actions, link_style, compiled.hi, attr_deps(ctx))
        indexing_tsets[link_style] = tset

    slis = []
    for lib in attr_deps(ctx):
        li = lib.get(SharedLibraryInfo)
        if li != None:
            slis.append(li)
    shlib_info = merge_shared_libraries(
        ctx.actions,
        deps = slis,
    )

    sos = []

    link_strategy = to_link_strategy(link_style)
    if link_group_info != None:
        own_binary_link_flags = []
        auto_link_groups = {}
        link_group_libs = {}
        link_deps = linkables(attr_deps(ctx))
        reduced_linkable_graph = reduce_linkable_graph(link_group_info.graph)
        linkable_graph_node_map = reduced_linkable_graph.nodes
        link_group_preferred_linkage = get_link_group_preferred_linkage(link_group_info.groups.values())

        # If we're using auto-link-groups, where we generate the link group links
        # in the prelude, the link group map will give us the link group libs.
        # Otherwise, pull them from the `LinkGroupLibInfo` provider from out deps.
        auto_link_group_specs = get_auto_link_group_specs(ctx, link_group_info)
        executable_deps = [d.linkable_graph.nodes.value.label for d in link_deps if d.linkable_graph != None]
        public_nodes = get_public_link_group_nodes(
            linkable_graph_node_map,
            link_group_info.mappings,
            executable_deps,
            None,
        )
        if auto_link_group_specs != None:
            linked_link_groups = create_link_groups(
                ctx = ctx,
                link_strategy = link_strategy,
                linkable_graph = reduced_linkable_graph,
                link_group_mappings = link_group_info.mappings,
                link_group_preferred_linkage = link_group_preferred_linkage,
                executable_deps = executable_deps,
                link_group_specs = auto_link_group_specs,
                public_nodes = public_nodes,
            )
            for name, linked_link_group in linked_link_groups.libs.items():
                auto_link_groups[name] = linked_link_group.artifact
                if linked_link_group.library != None:
                    link_group_libs[name] = linked_link_group.library
            own_binary_link_flags += linked_link_groups.symbol_ldflags

        else:
            # NOTE(agallagher): We don't use version scripts and linker scripts
            # for non-auto-link-group flow, as it's note clear it's useful (e.g.
            # it's mainly for supporting dlopen-enabled libs and extensions).
            link_group_libs = gather_link_group_libs(
                children = [d.link_group_lib_info for d in link_deps],
            )

        link_group_relevant_roots = find_relevant_roots(
            linkable_graph_node_map = linkable_graph_node_map,
            link_group_mappings = link_group_info.mappings,
            roots = get_dedupped_roots_from_groups(link_group_info.groups.values()),
        )

        roots = set(
            [d.linkable_graph.nodes.value.label for d in link_deps if d.linkable_graph != None] + link_group_relevant_roots,
        )
        is_executable_link = True
        pic_behavior = PicBehavior("supported")
        exec_linkables = collect_linkables(
            reduced_linkable_graph,
            is_executable_link,
            link_strategy,
            link_group_preferred_linkage,
            pic_behavior,
            roots,
        )
        build_context = BuildLinkGroupsContext(
            public_nodes = public_nodes,
            linkable_graph = reduced_linkable_graph,
            link_groups = link_group_info.groups,
            link_group_mappings = link_group_info.mappings,
            link_group_preferred_linkage = link_group_preferred_linkage,
            link_strategy = link_strategy,
            pic_behavior = pic_behavior,
            link_group_libs = {name: (lib.label, lib.shared_link_infos) for name, lib in link_group_libs.items()},
            prefer_stripped = False,
            prefer_optimized = False,
        )
        labels_to_links = get_filtered_labels_to_links_map(
            link_group = None,
            linkables = exec_linkables,
            is_executable_link = True,
            build_context = build_context,
            force_static_follows_dependents = True,
        )

        # NOTE: Our Haskell DLL support impl currently links transitive haskell
        # deps needed by DLLs which get linked into the main executable as link-
        # whole.  To emulate this, we mark Haskell rules with a special label
        # and traverse this to find all the nodes we need to link whole.
        public_nodes = []
        if ctx.attrs.link_group_public_deps_label != None:
            public_nodes = get_transitive_deps_matching_labels(
                linkable_graph_node_map = linkable_graph_node_map,
                label = ctx.attrs.link_group_public_deps_label,
                roots = link_group_relevant_roots,
            )

        link_infos = []
        link_infos.append(
            LinkInfo(
                pre_flags = own_binary_link_flags,
            ),
        )
        link_infos.extend(get_filtered_links(labels_to_links.map, set(public_nodes)))
        infos = LinkArgs(infos = link_infos)

        link_group_ctx = LinkGroupContext(
            link_group_mappings = link_group_info.mappings,
            link_group_libs = link_group_libs,
            link_group_preferred_linkage = link_group_preferred_linkage,
            labels_to_links_map = labels_to_links.map,
            targets_consumed_by_link_groups = {},
        )

        for shared_lib in traverse_shared_library_info(shlib_info, transformation_provider = None):
            label = shared_lib.label
            if is_link_group_shlib(label, link_group_ctx):
                sos.append(shared_lib)

        # When there are no matches for a pattern based link group,
        # `link_group_mappings` will not have an entry associated with the lib.
        for _name, link_group_lib in link_group_libs.items():
            sos.extend(link_group_lib.shared_libs.libraries)

    else:
        nlis = []
        for lib in attr_deps(ctx):
            if enable_profiling:
                hli = lib.get(HaskellProfLinkInfo)
                if hli != None:
                    nlis.append(hli.prof_infos)
                    continue
            li = lib.get(MergedLinkInfo)
            if li != None:
                nlis.append(li)
        sos.extend(traverse_shared_library_info(shlib_info, transformation_provider = None))
        cxx_toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]
        infos = get_link_args_for_strategy(
            ctx.actions,
            ctx.label,
            cxx_toolchain.linker_info,
            nlis,
            to_link_strategy(link_style),
            prefer_stripped = False,
            transformation_spec_context = None,
        )

    # `link_ordering = "topological"`, not the default `"preorder"`: this
    # migration hands raw static-archive paths straight to the linker
    # (unlike a plain Cabal/GHC `-package`-based link, which computes
    # correct archive order itself from the package DB). buck2's own
    # LinkOrdering docs (prelude/linking/link_info.bzl) describe exactly
    # what a single left-to-right `ld` pass needs here: "topological sort,
    # such that nodes are listed after all nodes that have them as
    # descendants" - i.e. a shared descendant (e.g. `primitive`, needed by
    # both `fb-util` and `vector`, two independent direct deps of the same
    # binary) is positioned after *every* consumer that needs it, not just
    # whichever one's subtree the default preorder-with-first-occurrence-
    # wins traversal happens to reach first (which, for two *independent*
    # siblings that share a transitive dependency, has no way to know the
    # *other* sibling needs it too - producing "undefined reference" even
    # though the defining archive is genuinely present on the command
    # line, just too early for the second consumer). This is a real,
    # first-class ordering mode already implemented in buck2's own tset
    # machinery - not a workaround - haskell_binary_impl here just never
    # requested it, always taking the `unpack_link_args` default.
    link_args.add(cmd_args(unpack_link_args(infos, link_ordering = LinkOrdering("topological")), prepend = "-optl"))

    link.add(
        at_argfile(
            actions = ctx.actions,
            name = "args.haskell_link_argsfile",
            args = link_args,
            allow_args = True,
        )
    )

    symlink_dir = None
    if link_style == LinkStyle("shared") or link_group_info != None:
        # Built *before* the link action below (not after, as this used to
        # be) so its real, build-time-valid path can be passed to the linker
        # as `-rpath-link` right now - see the comment on that flag below for
        # why this reordering is load-bearing, not cosmetic.
        sos_dir = "__{}__shared_libs_symlink_tree".format(ctx.attrs.name)
        symlink_dir = create_shlib_symlink_tree(
            actions = ctx.actions,
            out = sos_dir,
            shared_libs = sos,
        )
        rpath_ref = get_rpath_origin(get_cxx_toolchain_info(ctx).linker_info.type)
        rpath_ldflag = "-Wl,{}/{}".format(rpath_ref, sos_dir)

        # `--disable-new-dtags`: without it, GNU ld's default emits the
        # modern DT_RUNPATH tag for `-rpath`, which the dynamic loader only
        # consults when resolving *this object's own* direct NEEDED entries
        # - it is deliberately not inherited by anything loaded as a result
        # of it. That's invisible for a single-hop native shared dependency
        # (executable -> core.so -> rts.so: core.so's own link already
        # embeds a direct reference to rts.so, so the executable's runpath
        # is never even needed for that hop) but breaks a multi-hop chain
        # like executable -> db.so -> rocksdb.so -> storage.so: none of
        # db.so/rocksdb.so carry any rpath of their own (nothing in this
        # migration's cxx_library()/haskell_library() link steps sets one),
        # so once the executable's own DT_RUNPATH resolves db.so, nothing
        # is left to tell the loader where storage.so lives when resolving
        # *rocksdb.so's* NEEDED entries - "cannot open shared object file"
        # at process startup despite the .so genuinely being right there in
        # the symlink tree that got built. The legacy DT_RPATH tag doesn't
        # have this limitation: the loader folds every RPATH it encounters
        # into one global search list used for the *whole* transitive
        # resolution, not just the object that carries it - exactly what a
        # dependency chain of arbitrary depth needs, without requiring every
        # library along the way to separately carry its own rpath.
        link.add("-optl", "-Wl,--disable-new-dtags")
        link.add("-optl", "-Wl,-rpath", "-optl", rpath_ldflag)

        # `-rpath` (above) only takes effect once the *finished binary* runs
        # - it's a token ($ORIGIN/...) baked into the ELF's DT_RUNPATH,
        # meaningless to the linker invoked *right now* to produce that
        # binary, since there's no running process yet for $ORIGIN to refer
        # to. That's invisible for a single-hop shared dependency (e.g. this
        # binary -> rts.so directly): buck2's normal cxx linking machinery
        # already emits a direct, real `-L<dir> -l<name>` for anything in
        # `nlis` above, which the build-time linker can resolve on its own.
        # It breaks down for a *multi-hop* native shared-library chain (e.g.
        # this binary -> db.so -> rocksdb.so -> storage.so, where storage
        # is only ever a dependency-of-a-dependency, never listed directly
        # in any Haskell target's own `deps`): the linker, while resolving
        # rocksdb.so's own DT_NEEDED entry on storage.so to check whether it
        # actually provides the symbols db.so's own code references, has no
        # real, existing-right-now path to look in, and conservatively
        # reports those symbols undefined rather than assuming they'd
        # resolve at runtime. `-rpath-link` is the link-time-only counterpart
        # to `-rpath`: same directory, but as a real path valid *now*, purely
        # a search hint for the linker's own transitive-NEEDED resolution -
        # it doesn't get embedded in the output at all. Passing both this and
        # the $ORIGIN-relative `-rpath` fixes multi-hop native shared
        # dependencies generally, without needing to know in advance whether
        # any given target's dependency graph actually has one.
        link.add("-optl", cmd_args("-Wl,-rpath-link,", symlink_dir, delimiter = ""))

    ctx.actions.run(link, category = "haskell_link")

    if symlink_dir != None:
        run = cmd_args(output, hidden = symlink_dir)
    else:
        run = cmd_args(output)

    providers = [
        DefaultInfo(default_output = output),
        RunInfo(args = run),
    ]

    if indexing_tsets:
        providers.append(HaskellIndexInfo(info = indexing_tsets))

    return providers
