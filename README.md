A work in progress game engine.

## Demos

See the executables from `ghegin-games.cabal` in the `examples/` directory.
The few work in progress/unpolished executables are:
* `ocean-waves`
* `planets-core`
* `domain-warping`
* `fir-juliaset`, a port of FIR's juliaset to `ghengin`.
* `mandlebrot-set`

### Ocean waves

Based on https://dl.acm.org/doi/abs/10.1145/15922.15894


## Unique? features:

* Shader first -- the engine is design with custom shaders in the center, and
    a lot of compile time validation and runtime data is based on the shader
* Compile time validation of compatibility between the game defined materials,
    meshes and the game defined shader programs.
* It's written in Haskell
* ...

## Key ideas

I haven't had much time to write about this (even more so with the big
linear-types refactor on the way), but the key ideas are:

* `RenderPacket`s are things that get rendered, and each render packet is defined by:
    * A `Mesh`
    * a `Material`
    * and a `RenderPipeline`

* Meshes are vertices together with properties to influence the render of these
    vertices, and are parametrized by:
    * A type list describing the properties of each vertex in this mesh
    * (This is not yet implemented:) A type list describing the property
        bindings that describe this mesh and get bound to descriptor set #2 for
        each different mesh that is drawn.
        * Note that multiple render packets sharing the same mesh can be drawn
            while the mesh properties being still only bound once.

* Materials are group of properties that influence how all render packets
    sharing this Material are rendered; it is parametrized by:
    * A type list with the type of each property describing this material, which
        will get bound once to descriptor set #1 for every different material.
        * Note that multiple render packets with different meshes may share the
            same material, and the material properties will be shared across mesh draws without being rewritten
            * (Each material may get bound more than once, if there's no clear
                serialization of draw calls that ensures the material only needs
                to be bound once -- this has to due with heuristics in the
                render queue, I don't recall all the details)

* Render pipelines are group of properties and descriptions of render pipelines
    in graphics parlor, that define how all render packets that share this
    render pipeline are rendered (across different materials and meshes); it is
    parametrized by:
    * A type list describing the properties shared accross all render packets
        drawn with this render pipeline, that will be bound in descriptor set #0
    * A type-level complete description of the shader, which is the type of the
        shader program in the FIR shader language.

* The `Compatible` constraint must be satisfied in order to construct a render
    packet. This constraint validates, at compile time, that:
    * For the `Mesh` (see also `CompatibleMesh`)
        * The properties of each vertex match the vertice properties expected by the
            shader
        * (This is not yet implemented:) The mesh properties match the properties expected to be bound at
            descriptor set #2 in the shader
    * For the `Material` (see also `CompatibleMaterial`)
        * The properties of the material match the properties expected to be
            bound at descriptor set #1 by the shader
    * For the `RenderPipeline` (see also `CompatiblePipeline`)
        * The properties of the pipeline match the properties expected to be
            bound at descriptor set #0 by the shader

* ...

* The Core of the engine is abstract over the renderer implementation (through
    backpack), though we only have a vulkan implementation of the renderer, and
    the Core isn't yet fully standalone

* The Core of the engine is much like the Core in GHC: it strives to be a
    tiny but very expressive engine, that can represent in its completeness the
    full engine (which provides additional features not directly available in
    Core, but that can be expressed in it), for example:
    * The `Camera` construct is not part of Core, for it can be fully defined as
        a `RenderPipeline` property that gets bound in descriptor set #0 once
        per render pipeline, and some shader math. Of course, this ought to be
        provided as a plug and play capability in the full engine (say, one just
        has to import the Camera module, add it as a property of the render
        pipeline, and call the imported camera shader function in their own
        shader)
        * It's prettty good how in the shaders being written in Haskell one can
            easily use other engine-defined shader functions
    * ...

## Where's the action at?

The current demo is `planets`. To run it call:
```
cabal run planets
```

I'm trying to write a set of tutorials in some sort of book, based on an ocean
simulation, though progress has been slow as I've been busy with the linear
types refactor.

Write ups:
* https://discourse.haskell.org/t/monthly-update-on-a-haskell-game-engine/5515

### Notes 

Module dependencies:
```
find src/ ghengin-core/ ghengin-core-indep/ ghengin-vulkan/ -name '*.hs' | xargs graphmod -q -p | dot -T svg -o mods.svg
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

## ghengin-core

What `ghengin-core` does and does not:

- Does not implement a game-loop, nor a time step strategy
- Does not implement a scene-graph
- Nor (game) object positioning in terms of so-called "world coordinates"
- Does not provide a camera
- Does not manage game objects/entities in any way (no ECS, no FRP, actually, no concept of game object whatsover)
- Does not have a UI abstraction, but will allow renderpasses to be managed in such a way that one can add outside of Core, e.g., a dear-imgui render pass
- Has an (editable) Render Queue with the render packets (meshes + properties + material properties + pipeline with shader) that are rendered every frame
- Can express all of the above things it "does not" do with the existing concepts and combinators
- Handles double-buffering (eventually configurable?) semantics for the 'render' function, i.e. blocks after drawing a second frame
    - Actually, it's the renderer implementation that handles this

## Add-ons

These add-ons exist as separate packages, and are all included in `ghengin`, the
batteries included engine. These also attempt to be somewhat independent from
ghengin-core when possible.

- `ghengin-scene-graph`, which defines a scene-graph and world coordinate space
    with objects related in a hieararchy with properties defined relative to
    their parents (i.e. a scene, in its usual meaning)
- `ghengin-camera`, a camera object, shader, and update function (i.e. a camera, in its usual meaning)
- `ghengin-models`, to load and render 3D models
- `ghengin-lighting`, that provides lighting functions/models like the Blinn-Phong model
- `ghengin-dearimgui`, for UIs based on ghengin

## Ghengin

`ghengin` provides game-development abstractions on top of `ghengin-core`, and
is more developer friendly in the sense that it *does not* require linear types

## Other design ideas

- The `render/draw` function takes an action which "draws" things, which,
    depending on the implementation, either batches the drawcall or actually
    makes the draw call.
