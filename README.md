A work in progress game engine.

## Demos

### Ocean waves

Based on https://dl.acm.org/doi/abs/10.1145/15922.15894


## Unique features:

* Shader focused -- the engine is design with custom shaders in the center, and
    a lot of compile time validation and runtime data is based on the shader
* Compile time validation of compatibility between the game defined materials,
    meshes and the game defined shader programs.
* It's written in Haskell


The current demo is `planets`. To run it call:
```
cabal run planets
```

Write ups:
* https://discourse.haskell.org/t/monthly-update-on-a-haskell-game-engine/5515

### Notes 

Module dependencies:
```
find src/ -name '*.hs' | xargs graphmod -q -p | dot -T svg -o mods.svg
```

General dependencies:
```
cabal build -j --ghc-options=-fwrite-ide-info
calligraphy Ghengin.Core.* --exports-only --collapse-data --collapse-classes --output-stdout | dot -T svg -o img.svg -Kfdp
```
Debugging segfaults:

Compile with ghc-options: `-rtsopts` and use `+RTS -C0` to disable timer clock
something garbage collection (look the flag up).
Also, use flag +dev when developing
```sh
# try
$(cabal list-bin planets) +RTS -C0
# also, if planets is compiled with -g
lldb -- $(cabal list-bin planets) +RTS -C0
```

More resources:
* https://developer.nvidia.com/blog/vulkan-dos-donts/
* https://zeux.io/2020/02/27/writing-an-efficient-vulkan-renderer/
* https://www.intel.com/content/www/us/en/developer/articles/training/api-without-secrets-introduction-to-vulkan-part-6.html
* https://arm-software.github.io/vulkan_best_practice_for_mobile_developers/samples/performance/descriptor_management/descriptor_management_tutorial.html
* [Creating the Art of ABZU](https://www.youtube.com/watch?v=l9NX06mvp2E)

WIP: Packaging
---

Need to work out a packaging library.
Notes:
* Vulkan dynamic library could be included in the bundle if the executable
    link path is changed (see `otool -L` and `install_name_tool`)
* File system resources must be accessed through some hoops or it all goes to
    shit. It would be good to wrap this in a library which uses CPP to determine
    whether to use MacOS's CoreFoundation things, or Android's bundles, or just
    directly gets the resource

On MacOS:

App bundle structure:
https://www.lunarg.com/wp-content/uploads/2022/05/The-State-of-Vulkan-on-Apple-15APR2022.pdf

Vulkan dynamically linked libraries should be included in the bundle, use
`install_name_tool` to set location?

Have spent quite some time but haven't got the .app to open yet, despite the
executable working otherwise...

Clues:
* Ghengin game doesn't work
* Simple gloss app manually bundled works
* SDL Vulkan Triangle manually bundled also works, despite the otool -L showing @rpath/libvulkan.1.dylib
* GLFW-b OpenGL demo manually bundled works trivially
* FIR SDL Vulkan demo trivially works too
* GLFW + vulkan example in glfw source github repository also works trivially
* SDL + vulkan + dear-imgui.hs example fails to open!!
* GLFW + OpenGL + dear-imgui.hs example works trivially 

Current conclusion: Vulkan + Dear-ImGui.hs combination has something that makes
it fail to open when bundled in a .app

However, building the vulkan + glfw + dear-imgui example from the original C++ source succeeds.
So, is the error in the haskell dear-imgui + vulkan ?

Turns out the vulkan + dear-imgui doesn't work because of an asset it depends
on, but if I hardcode the path then it works.

So, my engine's apps are the only ones not working with
RBSStateCapture remove item called for untracked item

Fixed: after narrowing down the commit in which my app became unrunnable, I
realized the output logging file was causing the silent crash-on-start.  What a
truly awful, painful experience.

### Note on resources

The assets folder is copied to the bundle, so all resources should be in assets

