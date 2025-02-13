{ pkgs ?
  import <nixpkgs> {
    overlays = [
      (import ./nix/vulkan-validation-layers-overlay.nix)
    ]; }}:
with pkgs;
mkShell {
  name = "ghengin";
  packages = [
    haskell.compiler.ghc910
    haskell.packages.ghc910.cabal-install
    pkg-config
    zlib
    glfw
    moltenvk
    vulkan-headers
    vulkan-loader
    vulkan-validation-layers
    vulkan-tools      # <-- vulkaninfo
  ];

  buildInputs = with pkgs; [
    glfw
  ];

  # See "Set up the runtime environment manually"
  # in https://vulkan.lunarg.com/doc/sdk/1.4.304.1/mac/getting_started.html
  DYLD_LIBRARY_PATH="${glfw}/lib:${vulkan-loader}/lib";

  # Tell the Vulkan Loader where to find a Vulkan driver:
  VK_ICD_FILENAMES="${moltenvk}/share/vulkan/icd.d/MoltenVK_icd.json";

  # Not really used, I think. But here are the headers.
  VULKAN_SDK = "${vulkan-headers}";

  # Validation layers
  VK_LAYER_PATH = "${vulkan-validation-layers}/share/vulkan/explicit_layer.d";
}
