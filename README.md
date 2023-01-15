A work in progress game engine.

Unique features:

* Shader focused -- the engine is design with custom shaders in the center, and
    a lot of compile time validation and runtime data is based on the shader
* Compile time validation of compatibility between the game defined materials
    and the game defined shader programs.
* It's written in Haskell


The current demo is `planets`. To run it call:
```
cabal run planets
```

Write ups:
* https://discourse.haskell.org/t/monthly-update-on-a-haskell-game-engine/5515

More resources:
* https://developer.nvidia.com/blog/vulkan-dos-donts/
* https://zeux.io/2020/02/27/writing-an-efficient-vulkan-renderer/
* https://www.intel.com/content/www/us/en/developer/articles/training/api-without-secrets-introduction-to-vulkan-part-6.html
* https://arm-software.github.io/vulkan_best_practice_for_mobile_developers/samples/performance/descriptor_management/descriptor_management_tutorial.html
* [Creating the Art of ABZU](https://www.youtube.com/watch?v=l9NX06mvp2E)

WIP: Packaging
---

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

