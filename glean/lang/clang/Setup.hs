{-
  Copyright (c) 2013, Benjamin S. Scarlet and Google Inc.

  This a slight adaptation of the llvm-hs Setup.hs:
  https://github.com/llvm-hs/llvm-hs/blob/llvm-12/llvm-hs/Setup.hs

  # @lint-ignore-every LICENSELINT HLINT LINEWRAP
-}

{-# LANGUAGE CPP, FlexibleInstances, TupleSections #-}
import Control.Exception (SomeException, try)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Distribution.PackageDescription hiding (buildInfo, includeDirs)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import Distribution.System
import qualified Distribution.Types.Executable as Exe
import System.Environment

-- define these selectively in C files (we are _not_ using HsFFI.h),
-- rather than universally in the ccOptions, because HsFFI.h currently defines them
-- without checking they're already defined and so causes warnings.
uncheckedHsFFIDefines :: [String]
uncheckedHsFFIDefines = ["__STDC_LIMIT_MACROS"]

llvmVersions :: [Version]
llvmVersions = map mkVersion $ concat
  [ concat
    [ [ [a, b, c]
      | c <- versions_c
      ] ++ [[a, b]]
    | b <- versions_b
    ] ++ [[a]]
  | a <- versions_a
  ]

  where versions_a = [11, 12]
        versions_b = [0, 1]
        versions_c = [0, 1]

-- Ordered by decreasing specificty so we will prefer llvm-config-9.0
-- over llvm-config-9 over llvm-config.
llvmConfigNames :: [String]
llvmConfigNames = reverse versionedConfigs ++ ["llvm-config"]
  where
    versionedConfigs =
      map
        (\vs -> "llvm-config-" ++ intercalate "." (map show $ versionNumbers vs))
        llvmVersions

findJustBy :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findJustBy f (x:xs) = do
  x' <- f x
  case x' of
    Nothing -> findJustBy f xs
    j -> return j
findJustBy _ [] = return Nothing

llvmProgram :: Program
llvmProgram = (simpleProgram "llvm-config") {
  programFindLocation = \v p -> findJustBy (\n -> programFindLocation (simpleProgram n) v p) llvmConfigNames,
  programFindVersion = \verbosity path ->
    let
      stripVcsSuffix = takeWhile (\c -> isDigit c || c == '.')
      trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    in findProgramVersion "--version" (stripVcsSuffix . trim) verbosity path
 }

getLLVMConfig :: ConfigFlags -> IO ([String] -> IO String)
getLLVMConfig confFlags = do
  let verbosity = fromFlag $ configVerbosity confFlags
  (program, _, _) <- requireProgramVersion verbosity llvmProgram
                     llvmVersionsRange
                     (configPrograms confFlags)
  return $ getProgramOutput verbosity program

  where llvmVersionsRange = foldr1 unionVersionRanges [ thisVersion v | v <- llvmVersions ]

addToLdLibraryPath :: String -> IO ()
addToLdLibraryPath path = do
  let (ldLibraryPathVar, ldLibraryPathSep) =
        case buildOS of
          OSX -> ("DYLD_LIBRARY_PATH",":")
          _ -> ("LD_LIBRARY_PATH",":")
  v <- try $ getEnv ldLibraryPathVar :: IO (Either SomeException String)
  setEnv ldLibraryPathVar (path ++ either (const "") (ldLibraryPathSep ++) v)

addLLVMToLdLibraryPath :: ConfigFlags -> IO ()
addLLVMToLdLibraryPath confFlags = do
  llvmConfig <- getLLVMConfig confFlags
  [libDir] <- liftM lines $ llvmConfig ["--libdir"]
  addToLdLibraryPath libDir

-- | These flags are not relevant for us and dropping them allows
-- linking against LLVM build with Clang using GCC
ignoredCxxFlags :: [String]
ignoredCxxFlags = ["-fcolor-diagnostics"] ++ map ("-D" ++) uncheckedHsFFIDefines

ignoredCFlags :: [String]
ignoredCFlags = ["-fcolor-diagnostics"]

-- | Header directories are added separately to configExtraIncludeDirs
isIncludeFlag :: String -> Bool
isIncludeFlag flag = "-I" `isPrefixOf` flag

isWarningFlag :: String -> Bool
isWarningFlag flag = "-W" `isPrefixOf` flag

isIgnoredCFlag :: String -> Bool
isIgnoredCFlag flag = flag `elem` ignoredCFlags || isIncludeFlag flag || isWarningFlag flag

isIgnoredCxxFlag :: String -> Bool
isIgnoredCxxFlag flag = flag `elem` ignoredCxxFlags || isIncludeFlag flag || isWarningFlag flag

main :: IO ()
main = do
  let origUserHooks = simpleUserHooks

  defaultMainWithHooks (origUserHooks {
    hookedPrograms = [ llvmProgram ],

    confHook = \(genericPackageDescription, hookedBuildInfo) confFlags -> do
      llvmConfig <- getLLVMConfig confFlags
      llvmCxxFlags <- do
        rawLlvmCxxFlags <- llvmConfig ["--cxxflags"]
        return . filter (not . isIgnoredCxxFlag) $ words rawLlvmCxxFlags
      let stdLib = maybe "stdc++"
                         (drop (length stdlibPrefix))
                         (find (isPrefixOf stdlibPrefix) llvmCxxFlags)
            where stdlibPrefix = "-stdlib=lib"
      includeDirs <- liftM lines $ llvmConfig ["--includedir"]
      libDirs <- liftM lines $ llvmConfig ["--libdir"]
      [llvmVersion] <- liftM lines $ llvmConfig ["--version"]
      let getLibs = liftM (map (fromJust . stripPrefix "-l") . words) . llvmConfig
          flags    = configConfigurationsFlags confFlags
          linkFlag = "--link-shared"
      libs       <- getLibs ["--libs", linkFlag]
      systemLibs <- getLibs ["--system-libs", linkFlag]

      let llvmBuildInfo = mempty {
            extraLibs = libs ++ stdLib : systemLibs
            }
          genericPackageDescription' = genericPackageDescription
            { condLibrary = do -- maybe monad
                libraryCondTree <- condLibrary genericPackageDescription
                return libraryCondTree {
                  condTreeData = condTreeData libraryCondTree <>
                                 mempty { libBuildInfo = llvmBuildInfo }
                  }
            , condExecutables = do -- list monad
                (name, exeCondTree) <- condExecutables genericPackageDescription
                return . (name,) $ exeCondTree {
                  condTreeData = condTreeData exeCondTree <>
                                 mempty { Exe.buildInfo = llvmBuildInfo }
                  }
            }
          configFlags' = confFlags {
            configExtraLibDirs = libDirs ++ configExtraLibDirs confFlags,
            configExtraIncludeDirs = includeDirs ++ configExtraIncludeDirs confFlags
           }
      addLLVMToLdLibraryPath configFlags'
      confHook simpleUserHooks (genericPackageDescription', hookedBuildInfo) configFlags',

    hookedPreProcessors =
      let origHookedPreprocessors = hookedPreProcessors origUserHooks
          newHsc buildInfo localBuildInfo componentLocalBuildInfo =
              PreProcessor {
                  platformIndependent = platformIndependent (origHsc buildInfo),
                  ppOrdering = \_ _ ms -> pure ms,
                  runPreProcessor = \inFiles outFiles verbosity -> do
                      llvmConfig <- getLLVMConfig (configFlags localBuildInfo)
                      llvmCFlags <- do
                          rawLlvmCFlags <- llvmConfig ["--cflags"]
                          return . filter (not . isIgnoredCFlag) $ words rawLlvmCFlags
                      let buildInfo' = buildInfo { ccOptions = "-Wno-variadic-macros" : llvmCFlags }
                      runPreProcessor (origHsc buildInfo') inFiles outFiles verbosity
              }
              where origHsc buildInfo' =
                      fromMaybe
                        ppHsc2hs
                        (lookup "hsc" origHookedPreprocessors)
                        buildInfo'
                        localBuildInfo
                        componentLocalBuildInfo
      in [("hsc", newHsc)] ++ origHookedPreprocessors,

    buildHook = \packageDesc localBuildInfo userHooks buildFlags ->
      do addLLVMToLdLibraryPath (configFlags localBuildInfo)
         buildHook origUserHooks packageDesc localBuildInfo userHooks buildFlags,

    testHook = \args packageDesc localBuildInfo userHooks testFlags ->
      do addLLVMToLdLibraryPath (configFlags localBuildInfo)
         testHook origUserHooks args packageDesc localBuildInfo userHooks testFlags
   })
