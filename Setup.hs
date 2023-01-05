import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = (\a b c d -> putStrLn "The print works..." >> appBundleBuildHook guiApps a b c d) -- no op on other oses
  }

guiApps :: [MacApp]
guiApps =
  [ MacApp
    { appName = "Planets"
    , appIcon = Just "planets.icns"
    , appPlist = Nothing -- Build a default Info.plist for the icon.
    , resources = [] -- No other resources.
    , otherBins = [] -- No other binaries.
    , appDeps = DoNotChase -- Try changing to ChaseWithDefaults
    }
  ]
