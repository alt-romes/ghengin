import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = appBundleBuildHook guiApps -- no op on other oses
  }


guiApps :: [MacApp]
guiApps =
  [ MacApp
    -- very important: the name of the mac app must match the name of the executable
    { appName = "planets"
    , appIcon = Just "planets.icns"
    , appPlist = Nothing -- Build a default Info.plist for the icon.
    , resources = [] -- No other resources.
    , otherBins = [] -- No other binaries.
    , appDeps = DoNotChase -- Try changing to ChaseWithDefaults
    }
  ]
