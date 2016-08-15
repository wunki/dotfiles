DOTFILES 	:= $(PWD)
UNAME 	:= $(shell uname -s)

# These files are different per machine
xmobar=$(DOTFILES)/xmonad/${UNAME}-xmobarrc
gtkrc=$(DOTFILES)/gtk/${UNAME}-gtkrc.mine
gtk2=$(DOTFILES)/gtk/${UNAME}-gtkrc-2.0
tmux=$(DOTFILES)/tmux/${UNAME}-tmux.conf

all:: vim emacs bin zsh bash tmux xmonad xorg gtk mpv conkeror ghc

print-%: ; @echo $*=$($*)


vim::
	@ln -fs $(DOTFILES)/vim/vimrc									${HOME}/.vimrc
	@ln -fns $(DOTFILES)/vim											${HOME}/.vim
	@ln -fs $(DOTFILES)/vim/ycm_extra_conf.py			${HOME}/.ycm_extra_conf.py
	@echo Vim is symlinked.

nvim::
	@test -d ${HOME}/.config || mkdir						${HOME}/.config
	@ln -fns $(DOTFILES)/nvim										${HOME}/.config/nvim
	@ln -fs ${HOME}/.config/nvim/init.vim				${HOME}/.config/init.vim
	@echo NeoVim is symlinked.

emacs::
	@ln -fs $(DOTFILES)/emacs/emacs.el					${HOME}/.emacs
	@ln -fns $(DOTFILES)/emacs									${HOME}/.emacs.d
	@echo Emacs is symlinked.

bin::
	@ln -fns $(DOTFILES)/bin									${HOME}/.bin
	@echo bin files are linked to homedir.

bash::
	@ln -fs $(DOTFILES)/bash/bash_profile			${HOME}/.bash_profile
	@ln -fs $(DOTFILES)/bash/bashrc						${HOME}/.bashrc
	@ln -fs $(DOTFILES)/bash/bash_aliases			${HOME}/.bash_aliases
	@echo Bash is symlinked.

fish::
	@test -d ${HOME}/.config || mkdir					${HOME}/.config
	@ln -fns $(DOTFILES)/fish									${HOME}/.config/fish
	@echo Fish is symlinked.

tarsnap::
	@ln -fs $(DOTFILES)/tarsnap/tarsnapperrc	${HOME}/.tarsnapperrc
	@ln -fs $(DOTFILES)/tarsnap/tarsnaprc			${HOME}/.tarsnaprc
	@echo tarsnap is symlinked.

tmux::
	@ln -fs $(tmux)					                    ${HOME}/.tmux.conf
	@ln -fs $(DOTFILES)/tmux/tmux-status.conf		${HOME}/.tmux-status.conf
	@echo tmux is symlinked.

tmuxinator::
	@ln -fns $(DOTFILES)/tmuxinator						${HOME}/.tmuxinator
	@echo tmuxinator is symlinked.

xmonad::
	@mkdir -p																	${HOME}/.xmonad
	@ln -fs $(xmobar)													${HOME}/.xmobarrc
	@ln -fns $(DOTFILES)/xmonad/xmonad.hs			${HOME}/.xmonad/xmonad.hs
	@echo XMonad is symlinked.

xorg::
	@ln -fs $(DOTFILES)/xorg/Xresources				${HOME}/.Xresources
	@ln -fs $(DOTFILES)/xorg/xinitrc					${HOME}/.xinitrc
	@ln -fns $(DOTFILES)/xorg/fonts.conf			${HOME}/.fonts.conf
	@echo Xorg is symlinked.

gtk::
	@ln -fs $(gtk2)														${HOME}/.gtkrc-2.0
	@ln -fs $(gtkrc)													${HOME}/.gtkrc.mine
	@ln -fns $(DOTFILES)/gtk/themes			  		${HOME}/.themes
	@ln -fs $(DOTFILES)/gtk/user-dirs.dirs		${HOME}/.config/user-dirs.dirs
	@echo GTK is symlinked.

mpv::
	@mkdir -p ${HOME}/.mpv
	@ln -fns $(DOTFILES)/mpv/config						${HOME}/.mpv/config
	@echo MPV is symlinked.

ghc::
	@ln -fs $(DOTFILES)/ghc/ghci 							${HOME}/.ghci
	@echo GHC is symlinked.

termite::
	@test -d ${HOME}/.config || mkdir					${HOME}/.config
	@ln -fns $(DOTFILES)/termite						${HOME}/.config/termite

