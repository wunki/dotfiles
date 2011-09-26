DOTFILES = $(PWD)
all:: vim emacs zsh tmux

vim::
	@ln -fs $(DOTFILES)/vim/vimrc        ${HOME}/.vimrc
	@ln -fns $(DOTFILES)/vim/vim         ${HOME}/.vim
	@echo Vim is symlinked.

emacs::
	@ln -fs $(DOTFILES)/emacs/emacs.el	${HOME}/.emacs
	@ln -fns $(DOTFILES)/emacs					${HOME}/.emacs.d
	@echo Emacs is symlinked.

zsh::
	@ln -fs $(DOTFILES)/zsh/zshrc ${HOME}/.zshrc
	@ln -fs $(DOTFILES)/zsh/zshalias ${HOME}/.zshalias
	@ln -fs $(DOTFILES)/zsh/zshenv ${HOME}/.zshenv
	@ln -fns $(DOTFILES)/zsh/oh-my-zsh ${HOME}/.oh-my-zsh
	@echo ZSH is symlinked.

mutt::
	@ln -fs $(DOTFILES)/mutt/muttrc ${HOME}/.muttrc
	@ln -fns $(DOTFILES)/mutt/mutt ${HOME}/.mutt
	@ln -fs $(DOTFILES)/mutt/lbdbrc ${HOME}/.lbdbrc
	@echo mutt is symlinked

tmux::
	@ln -fs $(DOTFILES)/tmux/tmux.conf ${HOME}/.tmux
	@echo tmux is symlinked.
