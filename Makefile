.PHONY: tags report
tags:
	fast-tags -Rv src/ ghengin-vulkan/ ghengin-core/ ghengin-core-indep/

report:
	hlint src/ ghengin-vulkan/ ghengin-core/ ghengin-core-indep/ --report || true
