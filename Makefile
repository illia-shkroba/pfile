install:
	stack install
	pfile --zsh-completion-script "$$(which pfile)" > /usr/share/zsh/site-functions/_pfile

sdist:
	stack haddock --haddock-for-hackage
	stack sdist --pvp-bounds both --test-tarball

upload-candidate:
	stack upload --documentation --pvp-bounds both --test-tarball --candidate .

upload:
	stack upload --documentation --pvp-bounds both --test-tarball .
