{-# LANGUAGE CPP #-}
module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath
import System.Environment (getEnv, setEnv)
import Distribution.Simple.LocalBuildInfo (installedPkgs)
import Distribution.Simple.PackageIndex (lookupDependency)
import Distribution.Package (Dependency, PackageName)
import Distribution.Version (anyVersion)
import Distribution.System (buildOS, OS(Windows, OSX))
import Distribution.InstalledPackageInfo
import System.IO.Error

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
_PackageName = mkPackageName
#else
_PackageName = PackageName
#endif

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { buildHook = myBuildHook })

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
