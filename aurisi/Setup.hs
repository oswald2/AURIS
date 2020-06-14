{-# LANGUAGE CPP #-}
module Main (main) where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath
import Debug.Trace
import System.Environment (getEnv, setEnv)
import Distribution.Simple.LocalBuildInfo (installedPkgs)
import Distribution.Simple.PackageIndex (lookupDependency)
import Distribution.Package (Dependency, PackageName)
import Distribution.Version (anyVersion)
import Distribution.System (buildOS, OS(Windows, OSX))
import Distribution.InstalledPackageInfo
import System.IO.Error

----------------------------------------
-- compatibility between Cabal 1 and Cabal 2

-- "If you have a custom-setup stanza, you should be able to use the MIN_VERSION_Cabal macro in your setup script."

-- cabal >=2.0.0.2
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
_PackageName = mkPackageName
#else
_PackageName = PackageName
#endif
main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { buildHook = myBuildHook, hookedPreProcessors = [("fl", ppFluidToHaskell)] })

myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
  let fltkhsDependency = lookupDependency (installedPkgs local_bld_info) (Dependency (_PackageName "fltkhs") anyVersion)
      keepBuilding = (buildHook simpleUserHooks) pkg_descr local_bld_info user_hooks bld_flags
  in
  case fltkhsDependency of
    [] -> keepBuilding
    (_,[]):_ -> keepBuilding
    (_, (packageInfo:_)):_ ->
       mapM_ (updateEnv "LIBRARY_PATH") (libraryDirs packageInfo) >>
       keepBuilding

fluidToHaskellProgram :: Program
fluidToHaskellProgram =
  (simpleProgram "fltkhs-fluidtohs") { programFindVersion = (\_ _ -> return Nothing) }

updateEnv :: String -> String -> IO ()
updateEnv env value = do
  old <- tryIOError (getEnv env)
  setEnv env ((either (const value)
                      (\old' -> value ++
                               (case buildOS of
                                  Windows -> ";"
                                  _ -> ":") ++
                               old'
                      )
                      old))

-- cabal >=2.0.0.2
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
ppFluidToHaskell :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppFluidToHaskell bi lbi cli =
#else
ppFluidToHaskell :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppFluidToHaskell bi lbi =
#endif
  PreProcessor
    { platformIndependent = True
    , runPreProcessor = \(inBaseDir, inRelativeFile) (outBaseDir, outRelativeFile) verbosity -> do
        (fluidToHaskellProg, _) <- requireProgram verbosity fluidToHaskellProgram (withPrograms lbi)
        rawSystemProgram verbosity fluidToHaskellProg
          ["--output-dir=" ++ outBaseDir, inBaseDir </> inRelativeFile]
    }
