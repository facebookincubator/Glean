// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.exceptions.DescriptorException;
import com.facebook.glean.descriptors.utils.TypeUtils;
import com.facebook.glean.schema.java_alpha.XRef;
import com.facebook.glean.schema.java_alpha.XRefKey;
import com.facebook.glean.schema.java_alpha.XRefTarget;
import com.facebook.glean.schema.javakotlin_alpha.MethodName;
import com.facebook.glean.schema.javakotlin_alpha.QName;
import com.facebook.glean.schema.src.ByteSpan;
import com.sun.source.tree.IdentifierTree;
import com.sun.source.tree.InstanceOfTree;
import com.sun.source.tree.MethodInvocationTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.NewClassTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreeScanner;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;

public class XRefDescriptor {

  public static final XRefDescriptorTreeScanner XREF_DESCRIPTOR_TREE_SCANNER =
      new XRefDescriptorTreeScanner();

  static class Utils {
    private static final Pattern IGNORE_XREFS_TO_FQNS;

    static {
      String pattern = "^java[.]lang";
      IGNORE_XREFS_TO_FQNS = Pattern.compile(pattern);
    }

    static boolean shouldIgnoreXref(QName qName) {
      return IGNORE_XREFS_TO_FQNS.matcher(qName.getKey().toString()).lookingAt();
    }

    static void describeXrefToTypeMirror(
        IndexerContext ic, TypeMirror typeMirror, Tree locationTree) {
      if (typeMirror == null) {
        return;
      }

      typeMirror =
          TypeUtils.IsArray(typeMirror)
              ? TypeUtils.UnWrapArray(typeMirror).componentType
              : typeMirror;
      if (!TypeUtils.IsPrimitive(typeMirror)) {
        // we only cross reference a declared type
        if (typeMirror.getKind() == TypeKind.DECLARED) {
          TypeElement typeElement = (TypeElement) ((DeclaredType) typeMirror).asElement();
          Utils.describeXrefToTypeElement(ic, typeElement, locationTree);
        }
      }
    }

    static void describeXrefToTypeElement(
        IndexerContext ic, TypeElement typeElement, Tree locationTree) {

      if (typeElement == null) {
        return;
      }

      QName qName = QNameDescriptor.describe(ic, typeElement, false);
      if (shouldIgnoreXref(qName)) {
        return;
      }

      if (ic.isCompilerGenerated(locationTree)) {
        return;
      }

      XRefTarget xRefTarget = XRefTarget.fromDefinition_(qName);
      List<ByteSpan> ranges = Arrays.asList(LocationDescriptor.getByteSpanOfTree(ic, locationTree));

      XRefKey xRefKey = new XRefKey.Builder().setTarget(xRefTarget).setRanges(ranges).build();
      XRef xRef = new XRef.Builder().setKey(xRefKey).build();

      ic.predicates.xRefPredicate.addFact(xRef);
    }
  }

  public static class XRefDescriptorTreeScanner extends TreeScanner<Void, IndexerContext> {

    @Override
    public Void visitMethod(MethodTree tree, IndexerContext ic) {
      // the cross reference is to the return type of the method
      // don't bother with variables, that is handled in visitVariable
      if (!(ic.isCompilerGenerated(tree))) {
        Tree returnTree = tree.getReturnType();
        if (returnTree != null) {
          Utils.describeXrefToTypeMirror(ic, TypeUtils.GetTypeMirror(ic, returnTree), returnTree);
        }
      }
      return super.visitMethod(tree, ic);
    }

    @Override
    public Void visitMethodInvocation(MethodInvocationTree tree, IndexerContext ic) {
      if (ic.isCompilerGenerated(tree)) {
        return super.visitMethodInvocation(tree, ic);
      }
      ExecutableElement executableElement =
          (ExecutableElement) ic.trees.getElement(ic.trees.getPath(ic.cu, tree));

      if (executableElement == null) {
        return super.visitMethodInvocation(tree, ic);
      }

      MethodName mName = MethodNameDescriptor.describe(ic, executableElement, false);
      if (Utils.shouldIgnoreXref(mName.getKey().getName())) {
        return super.visitMethodInvocation(tree, ic);
      }

      XRefTarget xRefTarget = XRefTarget.fromMethod_(mName);

      List<ByteSpan> ranges = Arrays.asList(LocationDescriptor.getByteSpanOfTree(ic, tree));

      XRefKey xRefKey = new XRefKey.Builder().setTarget(xRefTarget).setRanges(ranges).build();
      XRef xRef = new XRef.Builder().setKey(xRefKey).build();

      ic.predicates.xRefPredicate.addFact(xRef);

      return super.visitMethodInvocation(tree, ic);
    }

    @Override
    public Void visitNewClass(NewClassTree tree, IndexerContext ic) {
      if (ic.isCompilerGenerated(tree)) {
        return super.visitNewClass(tree, ic);
      }
      ExecutableElement executableElement =
          (ExecutableElement) ic.trees.getElement(ic.trees.getPath(ic.cu, tree));

      if (executableElement == null) {
        return super.visitNewClass(tree, ic);
      }

      MethodName ctorName = MethodNameDescriptor.describe(ic, executableElement, false);
      if (Utils.shouldIgnoreXref(ctorName.getKey().getName())) {
        return super.visitNewClass(tree, ic);
      }

      XRefTarget xRefTarget = XRefTarget.fromCtor_(ctorName);
      if (ic.isCompilerGenerated(tree.getIdentifier())) {
        List<ByteSpan> ranges = Arrays.asList(LocationDescriptor.getByteSpanOfTree(ic, tree));
        XRefKey xRefKey = new XRefKey.Builder().setTarget(xRefTarget).setRanges(ranges).build();
        XRef xRef = new XRef.Builder().setKey(xRefKey).build();

        ic.predicates.xRefPredicate.addFact(xRef);

      } else {
        List<ByteSpan> ranges =
            Arrays.asList(LocationDescriptor.getByteSpanOfTree(ic, tree.getIdentifier()));

        XRefKey xRefKey = new XRefKey.Builder().setTarget(xRefTarget).setRanges(ranges).build();
        XRef xRef = new XRef.Builder().setKey(xRefKey).build();

        ic.predicates.xRefPredicate.addFact(xRef);
      }

      return super.visitNewClass(tree, ic);
    }

    @Override
    public Void visitVariable(VariableTree tree, IndexerContext ic) {
      // the cross reference is to the type of this variable and to the variable if a field
      VariableElement variableElement =
          (VariableElement) ic.trees.getElement(ic.trees.getPath(ic.cu, tree));
      if (variableElement.getKind() == ElementKind.FIELD
          || variableElement.getKind() == ElementKind.ENUM_CONSTANT) {
        QName qName = QNameDescriptor.describe(ic, variableElement, false);
        if (!(Utils.shouldIgnoreXref(qName))) {
          XRefTarget xRefTarget = XRefTarget.fromField_(qName);
          List<ByteSpan> ranges = Arrays.asList(LocationDescriptor.getByteSpanOfTree(ic, tree));

          XRefKey xRefKey = new XRefKey.Builder().setTarget(xRefTarget).setRanges(ranges).build();
          XRef xRef = new XRef.Builder().setKey(xRefKey).build();

          ic.predicates.xRefPredicate.addFact(xRef);
        }
      }

      try {
        Utils.describeXrefToTypeMirror(ic, TypeUtils.GetTypeMirror(ic, tree), tree.getType());
      } catch (Exception e) {
        throw new DescriptorException(ic, tree, e.toString());
      }

      return super.visitVariable(tree, ic);
    }

    @Override
    public Void visitIdentifier(IdentifierTree tree, IndexerContext ic) {
      Element element = ic.trees.getElement(ic.trees.getPath(ic.cu, tree));
      if (element.getKind() == ElementKind.FIELD
          || element.getKind() == ElementKind.ENUM_CONSTANT) {

        // the cross reference is to the type of this variable and to the variable if a field
        VariableElement variableElement = (VariableElement) element;
        QName qName = QNameDescriptor.describe(ic, variableElement, false);
        if (!(Utils.shouldIgnoreXref(qName))) {
          XRefTarget xRefTarget = XRefTarget.fromField_(qName);
          List<ByteSpan> ranges = Arrays.asList(LocationDescriptor.getByteSpanOfTree(ic, tree));

          XRefKey xRefKey = new XRefKey.Builder().setTarget(xRefTarget).setRanges(ranges).build();
          XRef xRef = new XRef.Builder().setKey(xRefKey).build();

          ic.predicates.xRefPredicate.addFact(xRef);
        }
      }
      return super.visitIdentifier(tree, ic);
    }

    @Override
    public Void visitInstanceOf(InstanceOfTree tree, IndexerContext ic) {
      // the cross reference is to the type of this instance of check
      Utils.describeXrefToTypeMirror(
          ic, TypeUtils.GetTypeMirror(ic, tree.getType()), tree.getType());
      return super.visitInstanceOf(tree, ic);
    }
  }
}
