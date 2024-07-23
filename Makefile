install:
	stack install
	pfile --zsh-completion-script "$$(which pfile)" > /usr/share/zsh/site-functions/_pfile
