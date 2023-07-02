.PHONY: tags report
tags:
	fast-tags -Rv ghengin/ ghengin-vulkan/ ghengin-core/ ghengin-core-indep/ linear-utils/

report:
	hlint ghengin/ ghengin-vulkan/ ghengin-core/ ghengin-core-indep/ linear-utils/ --report || true
