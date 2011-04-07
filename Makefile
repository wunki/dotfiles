DOTFILES = $(PWD)
all:: git vim zsh mail

git::
	@ln -fs $(DOTFILES)/git/gitignore       ${HOME}/.gitignore
	@ln -fs $(DOTFILES)/git/gitconfig       ${HOME}/.gitconfig
	@echo Git is symlinked.

vim::
	@ln -fs $(DOTFILES)/vim/vimrc        ${HOME}/.vimrc
	@ln -fns $(DOTFILES)/vim/vim         ${HOME}/.vim
	@echo Vim is symlinked.

zsh::
	@ln -fs $(DOTFILES)/zsh/zshrc ${HOME}/.zshrc
	@ln -fs $(DOTFILES)/zsh/zshalias ${HOME}/.zshalias
	@ln -fs $(DOTFILES)/zsh/zshenv ${HOME}/.zshenv
	@ln -fns $(DOTFILES)/zsh/oh-my-zsh ${HOME}/.oh-my-zsh
	@echo ZSH is symlinked.

mail::
	@ln -fns $(DOTFILES)/mail/mutt ${HOME}/.mutt
	@ln -fns $(DOTFILES)/mail/imapfilter ${HOME}/.imapfilter
	@ln -fs $(DOTFILES)/mail/muttrc ${HOME}/.muttrc
	@ln -fs $(DOTFILES)/mail/sync_mailboxes ${HOME}/.bin/sync_mailboxes
	@ln -fs $(DOTFILES)/mail/lbdbrc ${HOME}/.lbdbrc
	@ln -fs $(DOTFILES)/mail/mailcap ${HOME}/.mailcap
	@ln -fs $(DOTFILES)/mail/msmtprc ${HOME}/.msmtprc
	@ln -fs $(DOTFILES)/mail/offlineimaprc ${HOME}/.offlineimaprc
	@echo Mail is symlinked.

tmux::
	@ln -fs $(DOTFILES)/tmux/tmux.conf ${HOME}/.tmux
	@echo tmux is symlinked.
