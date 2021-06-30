DOTFILES 	:= $(PWD)
UNAME 		:= $(shell uname -s)

# These files are different per machine
xmobar=$(DOTFILES)/xmonad/${UNAME}-xmobarrc
gtkrc=$(DOTFILES)/gtk/${UNAME}-gtkrc.mine
gtk2=$(DOTFILES)/gtk/${UNAME}-gtkrc-2.0
tmux=$(DOTFILES)/tmux/${UNAME}-tmux.conf

all:: alacritty vim neovim fish tmux xorg gtk

print-%: ; @echo $*=$($*)

alacritty::
	@ln -fs $(DOTFILES)/alacritty/alacritty.yml			${HOME}/.config/alacritty/alacritty.yml
	@echo Alacritty configuration has been linked. 

vim::
	@ln -fs $(DOTFILES)/vim/vimrc						${HOME}/.vimrc
	@ln -fns $(DOTFILES)/vim							${HOME}/.vim
	@ln -fs $(DOTFILES)/vim/ycm_extra_conf.py			${HOME}/.ycm_extra_conf.py
	@echo Vim is linked.

neovim::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fns $(DOTFILES)/neovim						${HOME}/.config/nvim
	@echo Neovim is symlinked.

fish::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fns $(DOTFILES)/fish						${HOME}/.config/fish
	@echo Fish is symlinked.

tmux::
	@ln -fs $(tmux)									${HOME}/.tmux.conf
	@ln -fs $(DOTFILES)/tmux/tmux-status.conf		${HOME}/.tmux-status.conf
	@echo tmux is symlinked.

xorg::
	@ln -fs $(DOTFILES)/xorg/Xresources				${HOME}/.Xresources
	@ln -fs $(DOTFILES)/xorg/xinitrc              	${HOME}/.xinitrc
	@ln -fns $(DOTFILES)/xorg/fonts.conf			${HOME}/.fonts.conf
	@echo Xorg is symlinked.

gtk::
	@ln -fs $(gtk2)									${HOME}/.gtkrc-2.0
	@ln -fs $(gtkrc)								${HOME}/.gtkrc.mine
	@ln -fns $(DOTFILES)/gtk/themes					${HOME}/.themes
	@ln -fs $(DOTFILES)/gtk/user-dirs.dirs			${HOME}/.config/user-dirs.dirs
	@echo GTK is symlinked.
