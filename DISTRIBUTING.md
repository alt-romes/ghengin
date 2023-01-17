Notes on distributing games built with Ghengin:

I want to distribute games built with my engine to multiple platforms. Starting
with MacOS, I want to copy the executable to a particular spot in the .app
directory hierarchy. Besides copying the executable, I want to run something
akin to install_name_tool -change @rpath/libvulkan.1.dylib
@executable_path/../Frameworks/libvulkan.1.dylib and I want to copy
libvulkan.1.version.dylib, libMoltenVk.dylib to and create a symlink from
libvulkan.1.dylib to libvulkan.1.version.dylib in that hierarchy directory.
Should this be done with cabal, or should I create an external tool with a
library like Shake?

I was also wondering whether it's possible to only run that at a "release"
stage, while normal builds don't require that packaging step


