.PHONY: tags report
tags:
	fast-tags -Rv ghengin-vulkan/ ghengin-core/ ghengin-core-indep/ linear-utils/ ghengin-games

report:
	hlint ghengin/ ghengin-vulkan/ ghengin-core/ ghengin-core-indep/ linear-utils/ --report || true
