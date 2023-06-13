// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean;

import com.facebook.glean.descriptors.ClassDescriptor;
import com.facebook.glean.descriptors.EnumDescriptor;
import com.facebook.glean.descriptors.ImportDescriptor;
import com.facebook.glean.descriptors.InterfaceDescriptor;
import com.facebook.glean.descriptors.PackageDescriptor;
import com.facebook.glean.descriptors.XRefDescriptor;
import com.facebook.glean.descriptors.debug.DebugUtils;
import com.facebook.glean.schema.java_alpha.ClassDeclaration;
import com.facebook.glean.schema.java_alpha.Definition;
import com.facebook.glean.schema.java_alpha.EnumDeclaration;
import com.facebook.glean.schema.java_alpha.FileXRefs;
import com.facebook.glean.schema.java_alpha.FileXRefsKey;
import com.facebook.glean.schema.java_alpha.ImportDeclaration;
import com.facebook.glean.schema.java_alpha.InterfaceDeclaration;
import com.facebook.glean.schema.java_alpha.PackageDeclaration;
import com.facebook.glean.schema.src.File;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.ImportTree;
import com.sun.source.tree.PackageTree;
import com.sun.source.util.JavacTask;
import com.sun.source.util.Plugin;
import com.sun.source.util.TaskEvent;
import com.sun.source.util.TaskListener;
import com.sun.source.util.TreeScanner;
import com.sun.source.util.Trees;
import com.sun.tools.javac.api.BasicJavacTask;
import com.sun.tools.javac.api.JavacTaskImpl;
import com.sun.tools.javac.util.Context;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.JavaFileManager;
import javax.tools.StandardLocation;

public class Indexer implements Plugin {

  private static final int MAX_OUTPUT_FILENAME_LENGTH = 160;

  @Override
  public String getName() {
    return "Indexer";
  }

  @Override
  public void init(JavacTask task, String... args) {

    Map<String, String> environmentVars = System.getenv();
    Map<String, Definition> definitions = new HashMap<>();
    boolean loggingEnabled = Boolean.parseBoolean(environmentVars.get("GLEAN_DEBUG_LOGGING"));

    task.addTaskListener(
        new TaskListener() {
          public void started(TaskEvent e) {}

          public void finished(TaskEvent e) {
            // Symbol resolution is complete after the analyze phase.
            if (e.getKind() != TaskEvent.Kind.ANALYZE) {
              return;
            }

            CompilationUnitTree cu = e.getCompilationUnit();
            IndexerContext ic = new IndexerContext(task, cu, loggingEnabled);

            Indexer.indexCompilationUnit(ic, Trees.instance(task), definitions);
            Indexer.flushArtifactsToDisk(ic);
          }
        });
  }

  private static FileObject getMetaINFOutputFile(JavacTask task, String fileName)
      throws IOException {
    Context context =
        (task instanceof JavacTaskImpl)
            ? ((JavacTaskImpl) task).getContext()
            : ((BasicJavacTask) task).getContext();
    JavaFileManager fileManager = context.get(JavaFileManager.class);
    return fileManager.getFileForOutput(StandardLocation.CLASS_OUTPUT, "META-INF", fileName, null);
  }

  private static void indexCompilationUnit(
      IndexerContext ic, Trees trees, Map<String, Definition> definitions) {
    // When the end position is low, then these compilation units have been
    // synthesized by the compiler and we are safe skipping them as the definitions
    // are contained in other non-synthesized compilation units we will process
    if (trees.getSourcePositions().getEndPosition(ic.cu, ic.cu) < 0) {
      return;
    }

    ic.cu.accept(
        new TreeScanner<Void, IndexerContext>() {

          @Override
          public Void visitClass(ClassTree tree, IndexerContext ic) {
            // Note: we do not anticipate building nested Declarations via visitor
            // So we are putting container as null. We will check this via testing
            ic.logger.indentedLog(
                "ClassDeclaration -- "
                    + tree.getSimpleName().toString()
                    + " -- kind: "
                    + tree.getKind().toString());
            switch (tree.getKind()) {
              case CLASS:
                ClassDeclaration classDeclaration =
                    ClassDescriptor.describe(ic, tree, null, definitions);
                break;
              case INTERFACE:
                InterfaceDeclaration interfaceDeclaration =
                    InterfaceDescriptor.describe(ic, tree, null, definitions);
                break;
              case ENUM:
                EnumDeclaration enumDeclaration =
                    EnumDescriptor.describe(ic, tree, null, definitions);
                break;
              default:
                break;
            }

            tree.accept(XRefDescriptor.XREF_DESCRIPTOR_TREE_SCANNER, ic);
            return super.visitClass(tree, ic);
          }

          @Override
          public Void visitImport(ImportTree tree, IndexerContext ic) {
            if (!ic.isCompilerGenerated(tree)) {
              ic.logger.indentedLog(
                  "ImportDeclaration -- "
                      + tree.getQualifiedIdentifier().toString()
                      + " -- isStatic(): "
                      + (tree.isStatic() ? "true" : "false"));
              ImportDeclaration importDeclaration = ImportDescriptor.describe(ic, tree);
            }
            return super.visitImport(tree, ic);
          }

          @Override
          public Void visitPackage(PackageTree tree, IndexerContext ic) {
            if (!ic.isCompilerGenerated(tree)) {
              ic.logger.indentedLog("PackageDeclaration -- " + tree.toString());
              PackageDeclaration packageDeclaration = PackageDescriptor.describe(ic, tree);
            }
            return super.visitPackage(tree, ic);
          }
        },
        ic);

    String sourceFilePath = ic.cu.getSourceFile().getName();

    ic.predicates.xRefPredicate.consolidate();
    FileXRefsKey fileXRefsKey =
        new FileXRefsKey.Builder()
            .setFile(new File.Builder().setKey(Utils.normalizePath(sourceFilePath)).build())
            .setXrefs(new ArrayList<>(ic.predicates.xRefPredicate.getFacts()))
            .build();
    FileXRefs fileXRefs = new FileXRefs.Builder().setKey(fileXRefsKey).build();
    ic.predicates.fileXRefsPredicate.addFact(fileXRefs);
    DebugUtils.debugLogFileXrefs(ic, fileXRefs);
    if (ic.loggingEnabled) {
      ic.logger.indentedLog(ic.predicates.serializeAll());
    }
  }

  private static void flushArtifactsToDisk(IndexerContext ic) {

    //
    // Write predicates to disk
    //

    // This is the full file path, we want just the filename with extension
    String sourceFilePath = ic.cu.getSourceFile().getName();
    File filePredicate =
        new File.Builder()
            .setKey(Utils.normalizePath(ic.cu.getSourceFile().toUri().getPath()))
            .build();
    // Add file to definition

    String sourceFileName = sourceFilePath.substring(sourceFilePath.lastIndexOf('/') + 1);
    String outputFileName = "glean-" + String.format("%05d", ic.ID) + "-" + sourceFileName;

    // BUCK caps max file length. Trim filename so it's below this threshold.
    String outputFileNameWithExtension =
        outputFileName.substring(0, Math.min(outputFileName.length(), MAX_OUTPUT_FILENAME_LENGTH))
            + ".json";

    try {
      FileObject file = Indexer.getMetaINFOutputFile(ic.task, outputFileNameWithExtension);
      Writer writer = file.openWriter();
      writer.write(ic.predicates.serializeAll());
      writer.close();
    } catch (IOException exp) {
      throw new RuntimeException(outputFileNameWithExtension + "---" + exp.getMessage());
    }

    //
    // Write logs to disk
    //

    if (!ic.loggingEnabled) {
      return;
    }

    outputFileName = "glean-log-" + String.format("%05d", ic.ID) + "-" + sourceFileName;
    outputFileNameWithExtension =
        outputFileName.substring(0, Math.min(outputFileName.length(), MAX_OUTPUT_FILENAME_LENGTH))
            + ".txt";

    try {
      FileObject file = Indexer.getMetaINFOutputFile(ic.task, outputFileNameWithExtension);
      Writer writer = file.openWriter();
      writer.write(ic.bufferedLogger.toString());
      writer.close();
    } catch (IOException exp) {
      throw new RuntimeException(outputFileNameWithExtension + "---" + exp.getMessage());
    }
  }
}
