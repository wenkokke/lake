{-# LANGUAGE NamedFieldPuns #-}

import Control.Exception (catch, handle, throwIO)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Traversable (for)
import Distribution.PackageDescription (ComponentName (..), ForeignLib (..), PackageDescription (..))
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (componentBuildDir)
import Distribution.Simple.Setup (BuildFlags (..), fromFlagOrDefault)
import Distribution.Simple.Utils (die', info)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (..), componentNameCLBIs)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (copyFile, doesDirectoryExist)
import System.Environment (getEnv)
import System.FilePath ((<.>))
import System.FilePath.Posix ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Info (os)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postBuild = \args buildFlags packageDescription localBuildInfo -> do
          let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
          maybeOutDir <- getOutDir verbosity
          for_ maybeOutDir $ \outDir -> do
            copyForeignHeader verbosity packageDescription localBuildInfo outDir
            copyForeignLib verbosity packageDescription localBuildInfo outDir
      }

copyForeignHeader :: Verbosity -> PackageDescription -> LocalBuildInfo -> FilePath -> IO ()
copyForeignHeader verbosity packageDescription localBuildInfo outDir = do
  info verbosity $ show localBuildInfo
  return ()

copyForeignLib :: Verbosity -> PackageDescription -> LocalBuildInfo -> FilePath -> IO ()
copyForeignLib verbosity packageDescription localBuildInfo outDir = do
  let PackageDescription {foreignLibs} = packageDescription
  for_ foreignLibs $ \foreignLib -> do
    let ForeignLib {foreignLibName} = foreignLib
    let foreignLibCLBIs = componentNameCLBIs localBuildInfo (CFLibName foreignLibName)
    for_ foreignLibCLBIs $ \foreignLibCLBI -> do
      let foreignLibBuildDir = componentBuildDir localBuildInfo foreignLibCLBI
      let foreignLibFileName = getForeignLibFileName foreignLibName
      let foreignLibBuildPath = foreignLibBuildDir </> foreignLibFileName
      let foreignLibInstallPath = outDir </> foreignLibFileName
      info verbosity $ "Installing " <> foreignLibFileName <> " to " <> outDir
      copyFile foreignLibBuildPath foreignLibInstallPath

getForeignLibFileName :: UnqualComponentName -> FilePath
getForeignLibFileName foreignLibName
  | System.Info.os == "mingw32" = libName <.> "dll"
  | System.Info.os == "darwin" = "lib" <> libName <.> "dylib"
  | otherwise = "lib" <> libName <.> "so"
  where
    libName = unUnqualComponentName foreignLibName

getOutDir :: Verbosity -> IO (Maybe FilePath)
getOutDir verbosity =
  handle doesNotExistErrorHandler unsafeGetOutDir
  where
    unsafeGetOutDir = do
      outDir <- getEnv "OUT_DIR"
      outDirExists <- doesDirectoryExist outDir
      unless outDirExists $
        die' verbosity $
          "OUT_DIR '" <> outDir <> "' does not exist"
      return $ Just outDir

    doesNotExistErrorHandler e =
      if isDoesNotExistError e then return Nothing else throwIO e
