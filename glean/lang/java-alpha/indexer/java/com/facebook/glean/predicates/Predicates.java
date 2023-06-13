// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.predicates;

import com.facebook.glean.schema.java_alpha.ClassDeclaration;
import com.facebook.glean.schema.java_alpha.ConstructorDeclaration;
import com.facebook.glean.schema.java_alpha.EnumDeclaration;
import com.facebook.glean.schema.java_alpha.FieldDeclaration;
import com.facebook.glean.schema.java_alpha.FileXRefs;
import com.facebook.glean.schema.java_alpha.ImportDeclaration;
import com.facebook.glean.schema.java_alpha.InterfaceDeclaration;
import com.facebook.glean.schema.java_alpha.LocalDeclaration;
import com.facebook.glean.schema.java_alpha.MethodDeclaration;
import com.facebook.glean.schema.java_alpha.PackageDeclaration;
import com.facebook.glean.schema.java_alpha.ParameterDeclaration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Predicates {

  public static final int JAVA_SCHEMA_VERSION = 1;

  public final Predicate<LocalDeclaration> localDeclarationPredicate =
      new ListPredicate<>("java.alpha.LocalDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<FieldDeclaration> fieldDeclarationPredicate =
      new ListPredicate<>("java.alpha.FieldDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<ParameterDeclaration> parameterDeclarationPredicate =
      new ListPredicate<>("java.alpha.ParameterDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<ConstructorDeclaration> constructorDeclarationPredicate =
      new ListPredicate<>("java.alpha.ConstructorDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<MethodDeclaration> methodDeclarationPredicate =
      new ListPredicate<>("java.alpha.MethodDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<InterfaceDeclaration> interfaceDeclarationPredicate =
      new ListPredicate<>("java.alpha.InterfaceDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<ClassDeclaration> classDeclarationPredicate =
      new ListPredicate<>("java.alpha.ClassDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<EnumDeclaration> enumDeclarationPredicate =
      new ListPredicate<>("java.alpha.EnumDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<ImportDeclaration> importDeclarationPredicate =
      new ListPredicate<>("java.alpha.ImportDeclaration." + JAVA_SCHEMA_VERSION);
  public final Predicate<PackageDeclaration> packageDeclarationPredicate =
      new ListPredicate<>("java.alpha.PackageDeclaration." + JAVA_SCHEMA_VERSION);
  public final XRefPredicate xRefPredicate =
      new XRefPredicate("java.alpha.XRef." + JAVA_SCHEMA_VERSION);
  public final Predicate<FileXRefs> fileXRefsPredicate =
      new ListPredicate<>("java.alpha.FileXRefs." + JAVA_SCHEMA_VERSION);

  private final List<Predicate> allPredicates =
      Arrays.asList(
          localDeclarationPredicate,
          fieldDeclarationPredicate,
          parameterDeclarationPredicate,
          constructorDeclarationPredicate,
          methodDeclarationPredicate,
          interfaceDeclarationPredicate,
          classDeclarationPredicate,
          enumDeclarationPredicate,
          importDeclarationPredicate,
          packageDeclarationPredicate,
          xRefPredicate,
          fileXRefsPredicate);

  public String serializeAll() {
    @SuppressWarnings("unchecked")
    List<String> serializedPredicates = new ArrayList<String>();
    for (Predicate p : allPredicates) {
      if (p.getFacts().isEmpty()) {
        continue;
      } else {
        serializedPredicates.add(p.serialize());
      }
    }

    StringBuilder sb = new StringBuilder();
    sb.append("[").append(String.join(", ", serializedPredicates)).append("]");
    return sb.toString();
  }
}
