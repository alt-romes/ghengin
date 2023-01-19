{-# LANGUAGE BlockArguments #-}
module Main where

import Data.List (isInfixOf)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

{-
Note [Packaging MacOS app]
~~~~~~~~~~~~~~~~~~~~~~~~~~

MacOS apps are a directory with a specific structure and the `.app` extension.

Because in Shake we cannot call `need` on directories, we use `Info.plist` (a
file that all apps must include) as the "staple file" which determines that an
app is or isn't built

-}

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

    want ["_build/Example.app/Contents/Info.plist"]

    -- TODO: Resources/vulkan/icd.d/MoltenVK_icd.json

    phony "clean" $ do
        putInfo "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/*.app/Contents/Info.plist" %> \out -> do

        -- Staple file
        -- (1) Build every other requirement of an app
        -- (2) Finally build the Info.plist

        -- (1)

        let frameworks = takeDirectory out </> "Frameworks"
            resources  = takeDirectory out </> "Resources"

        need [ frameworks </> "libvulkan.1.dylib"
             , frameworks </> "libMoltenVK.dylib"
             , resources  </> "vulkan" </> "icd.d" </> "MoltenVK_icd.json"
             ]

        -- (2)

        -- TODO
        cmd_ "touch" out

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


getVulkanLibPath :: Action FilePath
getVulkanLibPath = do
  StdoutTrim vulkanInfoPath <- cmd "which vulkaninfo"
  pure (takeDirectory vulkanInfoPath </> ".." </> "lib")

getVulkanSharePath :: Action FilePath
getVulkanSharePath = do
  StdoutTrim vulkanInfoPath <- cmd "which vulkaninfo"
  pure (takeDirectory vulkanInfoPath </> ".." </> "share")

