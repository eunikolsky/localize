.PHONY:
ghcid-test:
	@ghcid -c 'stack ghci localize:lib localize:test:localize-test --ghci-options=-fobject-code' -T main
