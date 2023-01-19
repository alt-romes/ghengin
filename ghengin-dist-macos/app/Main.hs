{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Data.List (isInfixOf)
import Data.String.Interpolate ( i )
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt


{-
Note [Packaging MacOS app]
~~~~~~~~~~~~~~~~~~~~~~~~~~

MacOS apps are a directory with a specific structure and the `.app` extension.

Because in Shake we cannot call `need` on directories, we use `Info.plist` (a
file that all apps must include) as the "staple file" which determines that an
app is or isn't built

-}

flags :: [OptDescr (Either String a)]
flags = [ 
        ]

infoPlist :: String -> String
infoPlist appName = [i|
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>CFBundleDevelopmentRegion</key>
	<string>English</string>
	<key>CFBundleExecutable</key>
	<string>#{appName}</string>
	<key>CFBundleName</key>
	<string>#{appName}</string>
	<key>CFBundleGetInfoString</key>
	<string>#{appName}</string>
	<key>CFBundleIconFile</key>
	<string>#{appName}.icns</string>
	<key>CFBundleIdentifier</key>
	<string>org.haskell.#{appName}</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundleLongVersionString</key>
	<string>1.0</string>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleShortVersionString</key>
	<string>1.0</string>
	<key>CFBundleSignature</key>
	<string>????</string>
	<key>CFBundleVersion</key>
	<string>1.0</string>
</dict>
</plist>
  |]
{-
	<key>NSHumanReadableCopyright</key>
	<string>Copyright (c) 2018 The Khronos Group Inc. LunarG Inc. All rights reserved.</string>
	<key>NSMainStoryboardFile</key>
	<string>Main</string>
	<key>NSPrincipalClass</key>
	<string>NSApplication</string>
	<key>CSResourcesFileMapped</key>
	<true/>
-}

main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles="_build"} flags $ \fvalues targets -> pure $ Just $ do 

    forM_ targets $ \case
      "clean" -> want ["clean"]
      appName -> want ["_build/" <> appName <> ".app/Contents/Info.plist"] -- Staple file

    phony "clean" $ do
        putInfo "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/*.app/Contents/Info.plist" %> \out -> do

        let appName = dropExtension $ dropTrailingPathSeparator (splitPath out !! 1)

        -- Staple file
        -- (1) Build every other requirement of an app
        -- (2) Finally build the Info.plist

        -- (1)

        let frameworks = takeDirectory out </> "Frameworks"
            resources  = takeDirectory out </> "Resources"
            macos      = takeDirectory out </> "MacOS"

        need [ frameworks </> "libvulkan.1.dylib"
             , frameworks </> "libMoltenVK.dylib"
             , resources  </> "vulkan" </> "icd.d" </> "MoltenVK_icd.json"
             , resources  </> appName <.> ".icns"
             , macos      </> appName
             ]

        -- (2)

        writeFile' out (infoPlist appName)

    "_build/*.app/Contents/MacOS/*" %> \out -> do

        -- To create the executable, run cabal build and list-bin to build it
        -- and get the resulting executable path
        cmd_ "cabal build" (takeFileName out)
        StdoutTrim executable <- cmd "cabal list-bin" (takeFileName out)

        -- Copy from cabal's dist folder to the app bundle
        copyFile' executable out

        -- Point to the bundled dynamic libraries instead of the rpath ones.
        cmd_ "install_name_tool -change @rpath/libvulkan.1.dylib @executable_path/../Frameworks/libvulkan.1.dylib" out

    "_build/*.app/Contents/Frameworks/libvulkan.1.dylib" %> \out -> do

        -- libvulkan.1.dylib is a symlink to libvulkan.1.version.dylib
        --
        -- We copy the actual vulkan lib to the target directory and create a
        -- symlink there

        vulkanLib <- getVulkanLibPath
        StdoutTrim actualVulkan <- cmd "readlink" (vulkanLib </> takeFileName out)

        copyFile' (vulkanLib </> actualVulkan) (takeDirectory out </> actualVulkan)
        cmd_ "ln" "-s" actualVulkan out

    "_build/*.app/Contents/Frameworks/libMoltenVK.dylib" %> \out -> do

        vulkanLib <- getVulkanLibPath
        copyFile' (vulkanLib </> takeFileName out) out

    "_build/*.app/Contents/Resources/vulkan/icd.d/MoltenVK_icd.json" %> \out -> do

        -- MoltenVK_icd.json must be copied from the vulkan share directory,
        -- and then edited such that "library_path" points to the correct place
        vulkanShare <- getVulkanSharePath

        let moltenVK_icd = vulkanShare </> "vulkan" </> "icd.d" </> takeFileName out

        contents <- readFileLines moltenVK_icd

        let contents' = map (\l -> if "\"library_path\"" `isInfixOf` l
                                      then "        \"library_path\": \"../../../Frameworks/libMoltenVK.dylib\","
                                      else l
                            ) contents

        writeFileLines out contents'

    "_build/*.app/Contents/Resources/*.icns" %> \out -> do

        copyFile' (takeFileName out) out


getVulkanLibPath :: Action FilePath
getVulkanLibPath = do
  StdoutTrim vulkanInfoPath <- cmd "which vulkaninfo"
  pure (takeDirectory vulkanInfoPath </> ".." </> "lib")

getVulkanSharePath :: Action FilePath
getVulkanSharePath = do
  StdoutTrim vulkanInfoPath <- cmd "which vulkaninfo"
  pure (takeDirectory vulkanInfoPath </> ".." </> "share")

