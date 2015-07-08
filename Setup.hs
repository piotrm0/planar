-- Derived from example from cabal-macosx

import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks {
    postBuild = appBundleBuildHook guiApps
    }

guiApps :: [MacApp]
guiApps = [MacApp "planar"
           (Just "icon.icns")
           (Just "Info.plist")
           ["estre.ttf"] -- No other resources.
           [] -- No other binaries.
           DoNotChase -- Try changing to ChaseWithDefaults
          ]
