.PHONY:
ghcid:
	@ghcid -c 'stack ghci'

.PHONY:
ghcid-test:
	@ghcid -c 'stack ghci localize:lib localize:test:localize-test --ghci-options=-fobject-code' -T main
