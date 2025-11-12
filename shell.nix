{ pkgs ?
  import <nixpkgs> {
    overlays = [
      (import ./nix/vulkan-validation-layers-overlay.nix)
    ];
    }}:
with pkgs;
mkShell ({
  name = "ghengin";
  packages = [
    haskell.compiler.ghc912
    haskell.packages.ghc912.cabal-install
    pkg-config
    zlib
    glfw
    # SDL2 # optionally, for debugging FIR. We don't support SDL2 yet
    vulkan-headers
    vulkan-loader
    vulkan-validation-layers
    vulkan-tools      # <-- vulkaninfo
  ]
  ++ (pkgs.lib.optionals pkgs.stdenv.isDarwin [
    moltenvk
  ])
  ++ (pkgs.lib.optionals pkgs.stdenv.isLinux [
    # Linux only pkgs
    freetype
    shaderc             # GLSL to SPIRV compiler - glslc
    # renderdoc           # Graphics debugger
    # tracy               # Graphics profiler
    vulkan-tools-lunarg # vkconfig

    # bindings-GLFW
    xorg.libX11
    xorg.libXi
    xorg.libXrandr
    xorg.libXxf86vm
    xorg.libXcursor
    xorg.libXinerama

    # debugging
    # gdb
    # libelf
    # libdwarf
    # elfutils
  ])
  ;

  # Not really used, I think. But here are the headers.
  VULKAN_SDK = "${vulkan-headers}";

  # Validation layers
  VK_LAYER_PATH = "${vulkan-validation-layers}/share/vulkan/explicit_layer.d";

} // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {

  # To find vulkan at load time on linux
  LD_LIBRARY_PATH = "${glfw}/lib:${freetype}/lib:${vulkan-loader}/lib:${vulkan-validation-layers}/lib";
} // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {

  # See "Set up the runtime environment manually"
  # in https://vulkan.lunarg.com/doc/sdk/1.4.304.1/mac/getting_started.html
  DYLD_LIBRARY_PATH="${glfw}/lib:${vulkan-loader}/lib";

  # Tell the Vulkan Loader where to find a Vulkan driver:
  VK_ICD_FILENAMES="${moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json";
})
