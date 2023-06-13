Glean Java Indexer

# Getting Started

## Indexing a target and writing to Glean
On a machine with a full fbsource checkout.

Indexing a target
```
cd ~/fbsource
buck run fbsource//fbandroid/tools/glean:index-target -- \
  --root-target fb4a \
  --clean \
  --output /tmp/indexed_jars.txt
```

Writing to glean
```
cd ~/fbsource/fbcode
buck run //glean/lang/java:write-to-glean -- \
  --repo fbandroid \
  --hash 007 \
  --service glean.write.test \
  --jars @/tmp/indexed_jars.txt
```

# The Indexer
The indexer is implemented as a Java compiler plugin. During compilation, the indexer gets access to the compiler AST, traverses it, and extracts type information relevant to the [Java glean schema](https://our.internmc.facebook.com/intern/diffusion/FBS/browse/master/fbcode/glean/schema/source/java.angle). Glean index files are written to the output jar of the library and is extracted in a post processing step and written to Glean.

The indexer implementation resides in `fbcode/glean/lang/java`.
- `indexer/java` contains the Indexer source. `Indexer.java` is the entry point.
- `indexer/examples` contains example classes to test the indexer. See the Buck target descriptions below for more info.

### Syncing to fbandroid
After making changes to the indexer in fbcode, you **must sync the changes to fbandroid**. This is necessary since buck targets in fbandroid cannot directly depend on fbcode targets. The sync is done via Dewey, is fully automated, and instant. After testing and committing your changes, run `buck run //glean/lang/java:sync-to-fbandroid`. This will build, test, sync, and verify the indexer works in fbandroid. **Commit the sync diff**.

# Overview of BUCK Targets

## fbcode/glean/lang/java
**fbcode//glean/lang/java:indexer-lib**
Core indexer code. Point of entry is Indexer.java. Indexer output files are written to the META-INF directory of the library's jar during compilation.

**fbcode//glean/lang/java:java**
Fully bundled indexer-lib jar. This jar contains all dependencies of indexer-lib in a single jar. See comments in TARGETS file for more details.

**fbcode//glean/lang/java:examples**
Example java library to index.
```
// index the library and print the output jar path
buck build fbcode//glean/lang/java:examples-java8 --show-output

// extract indexed JSON files from jar
mkdir -p /tmp/glean
cd /tmp/glean
cp <path to indexed jar> /tmp/glean/tmp.jar
unzip /tmp/glean/tmp.jar *.json

// load json files into local glean db
mkdir -p /tmp/glean/db_root
buck run fbcode//glean/shell:shell -- \
  --schema file \
  --db-root /tmp/glean/db_root

# inside glean shell
:load <path to index json file>
```

**fbcode//glean/lang/java:java-python-lib**
Python library containing all python scripts required for the java indexer.

**fbcode//glean/lang/java:write-to-glean**
Python binary to write indexed jar paths to Glean.

**fbcode//glean/lang/java:e2e-test**
End to end test of the indexer. This test compiles the indexer, indexes the examples library, writes to Glean, and verifies the write succeeded.
```
buck run fbcode//glean/lang/java:e2e-test
```

**fbcode//glean/lang/java:sync-to-fbandroid**
Syncs the fully bundled indexer jar to fbsource/fbandroid/tools/glean. This target builds the latest version of the indexer, uploads it to Dewey, updates the target definitions in fbandroid, and verifies that the new indexer functions. This is necessary since fbandroid targets cannot directly depend on fbcode targets. Remeber to commit the sync diff.

## fbsource/fbandroid/tools/glean
**fbsource//fbandroid/tools/glean:indexer-dewey-artifact**
Dewey target that points to the latest stable indexer release. The hash in this file is the fbcode revision the indexer was built on.

**fbsource//fbandroid/tools/glean:indexer-jar**
Buck prebuilt jar target that wraps the Dewey artifact. This is how we expose the Dewey artifact to the rest of Buck as a jar file.

**fbsource//fbandroid/tools/glean:glean**
This is the javac plugin target for the indexer. For any target to be indexed, it must include this target as a compiler plugin. See notes on the javac plugin decorator on how to do this easily.

**fbsource//fbandroid/tools/glean:helloworld-lib**
A dummy java library. During verify, the indexer indexes this library.

**fbsource//fbandroid/tools/glean:glean-python-lib**
Python library containing all python scripts required for the java indexer.

**fbsource//fbandroid/tools/glean:index-target**
Python binary that indexes a buck target and outputs all indexed jars to a file.
```
buck run fbsource//fbandroid/tools/glean:index-target -- \
  --root-target fb4a \
  --clean \
  --output /tmp/output.txt
```

**fbsource//fbandroid/tools/glean:verify-bin**
A python binary that verifies the indexer pointed to by indexer-jar executes as expected.

**fbsource//fbandroid/tools/glean:verify**
Genrule that runs verify-bin. If this rule builds, it means the fbandroid indexer is functioning as expected. You shouldn't need to invoke this target directly. It is automatically called during the sync process from fbcode. The sync will fail if verify fails.

### Java compiler plugin decorator
The glean indexer is implemented as a java compiler plugin. To make it easy to use, there is an easy way to insert a compiler plugin to the BUCK build process. The [javac plugin decorator](https://fburl.com/diffusion/g6c5tijr) modifies targets to invoke the specified compiler plugin during build. We use BUCK options to configure to the decorator:

```
-c javac.plugin=<buck target of compiler plugin>
// Glean indexing is not the only compiler plugin available. Checkout the jAST project to write powerful java codemods.
// To print the AST of a java source file check out fbsource//fbandroid/java/com/facebook/tools/jast/astprinter:ast_printer_plugin.

-c javac.shallow_target=<buck target>
// without this option, the plugin will be applied to the target being compiled, and all of its dependencies. This flag limits which target the plugin is applied to

-c javac.label=<string>
// A label to attach to all Buck targets the plugin runs on. This can be used in a buck query afterwards to find all indexed targets.

-c javac.random_seed=<int>
// Append a random value to the buck target. This forces buck to invalidate its cache and rebuild this target.

// example
buck build @fbandroid/mode/server fbsource//fbandroid/java/com/facebook/movies/graphql:graphql \
  -c javac.plugin=fbsource//fbandroid/tools/glean:glean \
  -c javac.shallow_target=fbsource//fbandroid/java/com/facebook/movies/graphql:graphql \
  -c javac.label=glean_indexed \
  -c javac.random_seed=1 \
  --local \
  --no-cache \
  --show-output
```

# Resources
- Java plugin docs: https://docs.oracle.com/javase/8/docs/jdk/api/javac/tree/com/sun/source/util/Plugin.html
- Dewey docs: https://our.internmc.facebook.com/intern/wiki/DeweyGuide/
