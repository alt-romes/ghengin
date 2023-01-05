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
