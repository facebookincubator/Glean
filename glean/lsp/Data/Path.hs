{-
  Copyright (c) 2023 Joseph Sumabat

  See glean/lsp/LICENSE
-}

module Data.Path (
  Path (path, Path, UncheckedPath),
  KnownPathKind (sPathKind),
  AbsPath,
  PathKind (..),
  RelPath,
  filePathToAbs,
  unsafeFilePathToAbs,
  filePathToRel,
  toFilePath,
  (</>),
  makeRelative,
  absToRel,
  (<.>),
  (-<.>),
  filePathToAbsThrow,
  uncheckedCoercePath,
  relToAbsThrow,
)
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import GHC.Stack (HasCallStack)
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath
import UnliftIO (stringException)

-- | Rel means may be relative or absolute, absolute means must be absolute
data PathKind = Rel | Abs

data SPathKind p where
  SRel :: SPathKind Rel
  SAbs :: SPathKind Abs

class KnownPathKind p where
  sPathKind :: SPathKind p

instance KnownPathKind Abs where
  sPathKind = SAbs

instance KnownPathKind Rel where
  sPathKind = SRel

newtype Path (p :: PathKind) = UncheckedPath {path :: FilePath}
  deriving (Show, Eq, Ord, Hashable, Aeson.FromJSON, Aeson.ToJSON)

uncheckedCoercePath :: Path p -> Path q
uncheckedCoercePath = coerce

instance IsString (Path Rel) where
  fromString = UncheckedPath

pattern Path :: FilePath -> Path p
pattern Path p <- UncheckedPath p

type AbsPath = Path Abs

type RelPath = Path Rel

toFilePath :: (HasCallStack) => Path p -> FilePath
toFilePath = (.path)

filePathToRel :: (HasCallStack) => FilePath -> RelPath
filePathToRel = UncheckedPath

filePathToAbs :: (HasCallStack, MonadIO m) => FilePath -> m AbsPath
filePathToAbs p = do
  absPath <- liftIO $ Dir.makeAbsolute p
  pure $ UncheckedPath absPath

unsafeFilePathToAbs :: (HasCallStack) => FilePath -> AbsPath
unsafeFilePathToAbs p
  | FilePath.isAbsolute p = UncheckedPath p
  | otherwise = error "unsafeOsPathToAbs: path is not absolute"

relToAbsThrow :: (MonadThrow m, HasCallStack) => RelPath -> m AbsPath
relToAbsThrow (UncheckedPath p) = filePathToAbsThrow p

filePathToAbsThrow :: (MonadThrow m, HasCallStack) => FilePath -> m AbsPath
filePathToAbsThrow p
  | FilePath.isAbsolute p = pure $ UncheckedPath p
  | otherwise = throwM (stringException $ "filepath was not absolute: " ++ p)

(</>) :: Path p -> Path Rel -> Path p
(UncheckedPath p) </> (UncheckedPath p') = UncheckedPath (p FilePath.</> p')

infixr 5 </>

(<.>) :: Path p -> String -> Path p
(UncheckedPath p) <.> ext = UncheckedPath (p FilePath.<.> ext)

(-<.>) :: Path p -> String -> Path p
(UncheckedPath p) -<.> ext = UncheckedPath (p FilePath.-<.> ext)

absToRel :: AbsPath -> RelPath
absToRel (UncheckedPath p) = UncheckedPath p

makeRelative :: Path p -> Path q -> Path Rel
makeRelative (UncheckedPath p) (UncheckedPath q) = UncheckedPath (FilePath.makeRelative p q)
