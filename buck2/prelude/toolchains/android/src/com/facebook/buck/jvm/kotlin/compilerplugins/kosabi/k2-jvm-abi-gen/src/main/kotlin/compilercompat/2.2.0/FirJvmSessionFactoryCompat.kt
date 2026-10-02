/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:Suppress("PackageLocationMismatch")

package com.facebook

import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.languageVersionSettings
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.deserialization.ModuleDataProvider
import org.jetbrains.kotlin.fir.extensions.FirExtensionRegistrar
import org.jetbrains.kotlin.fir.java.FirProjectSessionProvider
import org.jetbrains.kotlin.fir.session.FirJvmIncrementalCompilationSymbolProviders
import org.jetbrains.kotlin.fir.session.FirJvmSessionFactory
import org.jetbrains.kotlin.fir.session.FirSessionConfigurator
import org.jetbrains.kotlin.fir.session.FirSharableJavaComponents
import org.jetbrains.kotlin.fir.session.environment.AbstractProjectEnvironment
import org.jetbrains.kotlin.fir.session.environment.AbstractProjectFileSearchScope
import org.jetbrains.kotlin.load.kotlin.PackagePartProvider
import org.jetbrains.kotlin.name.Name

fun createLibrarySessionCompat(
    rootModuleName: Name,
    sessionProvider: FirProjectSessionProvider,
    moduleDataProvider: ModuleDataProvider,
    projectEnvironment: AbstractProjectEnvironment,
    extensionRegistrars: List<FirExtensionRegistrar>,
    librariesScope: AbstractProjectFileSearchScope,
    packagePartProvider: PackagePartProvider,
    languageVersionSettings: org.jetbrains.kotlin.config.LanguageVersionSettings,
    predefinedJavaComponents: FirSharableJavaComponents?,
): FirSession {
  val sharedLibrarySession =
      FirJvmSessionFactory.createSharedLibrarySession(
          rootModuleName,
          sessionProvider,
          projectEnvironment,
          extensionRegistrars,
          librariesScope,
          packagePartProvider,
          languageVersionSettings,
          predefinedJavaComponents,
      )
  return FirJvmSessionFactory.createLibrarySession(
      sessionProvider,
      sharedLibrarySession,
      moduleDataProvider,
      projectEnvironment,
      extensionRegistrars,
      librariesScope,
      packagePartProvider,
      languageVersionSettings,
      predefinedJavaComponents,
  )
}

fun createSourceSessionCompat(
    moduleData: FirModuleData,
    sessionProvider: FirProjectSessionProvider,
    javaSourcesScope: AbstractProjectFileSearchScope,
    projectEnvironment: org.jetbrains.kotlin.cli.jvm.compiler.VfsBasedProjectEnvironment,
    createIncrementalCompilationSymbolProviders:
        (FirSession) -> FirJvmIncrementalCompilationSymbolProviders?,
    extensionRegistrars: List<FirExtensionRegistrar>,
    configuration: CompilerConfiguration,
    predefinedJavaComponents: FirSharableJavaComponents?,
    needRegisterJavaElementFinder: Boolean,
    init: FirSessionConfigurator.() -> Unit,
): FirSession {
  return FirJvmSessionFactory.createSourceSession(
      moduleData,
      sessionProvider,
      javaSourcesScope,
      projectEnvironment,
      createIncrementalCompilationSymbolProviders,
      extensionRegistrars,
      configuration,
      predefinedJavaComponents = predefinedJavaComponents,
      needRegisterJavaElementFinder = needRegisterJavaElementFinder,
      init = init,
  )
}
