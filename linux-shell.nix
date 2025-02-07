with import <nixpkgs> {};

mkShell {
  name = "ghengin";
  packages = [
    haskell.compiler.ghc910
    cabal-install
    pkg-config
    glfw
    freetype
    vulkan-headers
    vulkan-loader
    vulkan-validation-layers
    vulkan-tools        # vulkaninfo
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
    gdb
    libelf
    libdwarf
    elfutils
  ];

  buildInputs = with pkgs; [
    glfw
    freetype
  ];

  LD_LIBRARY_PATH="${glfw}/lib:${freetype}/lib:${vulkan-loader}/lib:${vulkan-validation-layers}/lib";
  VULKAN_SDK = "${vulkan-headers}";
  VK_LAYER_PATH = "${vulkan-validation-layers}/share/vulkan/explicit_layer.d";
}
