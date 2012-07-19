DOTFILES = $(PWD)
all:: vim emacs zsh tmux xmonad xorg gtk pentadactyl

vim::
	@ln -fs $(DOTFILES)/vim/vimrc        ${HOME}/.vimrc
	@ln -fns $(DOTFILES)/vim/vim         ${HOME}/.vim
	@echo Vim is symlinked.

emacs::
	@ln -fs $(DOTFILES)/emacs/emacs.el	${HOME}/.emacs
	@ln -fns $(DOTFILES)/emacs					${HOME}/.emacs.d
	@ln -fs $(DOTFILES)/emacs/wl						  ${HOME}/.wl
	@ln -fs $(DOTFILES)/emacs/folders					${HOME}/.folders
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
	@touch ~/.muttrc-custom.conf
	@echo mutt is symlinked.

tmux::
	@ln -fs $(DOTFILES)/tmux/tmux.conf ${HOME}/.tmux
	@echo tmux is symlinked.

xmonad::
	@mkdir -p ${HOME}/.xmonad
	@ln -fs $(DOTFILES)/xmonad/xmobarrc ${HOME}/.xmobarrc
	@ln -fns $(DOTFILES)/xmonad/xmonad.hs ${HOME}/.xmonad/xmonad.hs
	@echo XMonad is symlinked.

xorg::
	@ln -fs $(DOTFILES)/xorg/Xdefaults ${HOME}/.Xdefaults
	@ln -fs $(DOTFILES)/xorg/Xresources ${HOME}/.Xresources
	@ln -fs $(DOTFILES)/xorg/xinitrc ${HOME}/.xinitrc
	@ln -fns $(DOTFILES)/xorg/urxvt-scripts ${HOME}/.urxvt-scripts
	@echo Xorg is symlinked.

gtk::  
	@ln -fs $(DOTFILES)/gtk/gtkrc-2.0 ${HOME}/.gtkrc-2.0
	@ln -fs $(DOTFILES)/gtk/gtkrc.mine ${HOME}/.gtkrc.mine
	@ln -fns $(DOTFILES)/gtk/themes ${HOME}/.themes
	@echo GTK is symlinked.

pentadactyl::
	@ln -fs $(DOTFILES)/pentadactylrc ${HOME}/.pentadactylrc
