self: super:

{
  # https://github.com/NixOS/nixpkgs/blob/nixos-24.11/pkgs/by-name/vu/vulkan-validation-layers/package.nix
  vulkan-validation-layers = super.vulkan-validation-layers.overrideAttrs {
    buildInputs = [
      super.glslang
      # robin-hood-hashing # this was imported into the original file,
      # what to do?
      super.spirv-headers
      super.spirv-tools
      super.vulkan-headers
      super.vulkan-utility-libraries
    ]
    ++ super.lib.optionals super.stdenv.hostPlatform.isLinux [
      super.wayland
      super.xorg.libX11
      super.xorg.libXau
      super.xorg.libXdmcp
      super.xorg.libXrandr
      super.xorg.libxcb
      super.libffi
    ];

    meta.platforms = super.lib.platforms.all;
  };
}
