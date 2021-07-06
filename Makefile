DOTFILES 	:= $(PWD)
UNAME 		:= $(shell uname -s)

# These files are different per machine
xmobar=$(DOTFILES)/xmonad/${UNAME}-xmobarrc
gtkrc=$(DOTFILES)/gtk/${UNAME}-gtkrc.mine
gtk2=$(DOTFILES)/gtk/${UNAME}-gtkrc-2.0
tmux=$(DOTFILES)/tmux/${UNAME}-tmux.conf

BSPWM_DIR=${HOME}/.config/bspwm

all:: alacritty vim neovim fish tmux xorg gtk

print-%: ; @echo $*=$($*)

alacritty::
	@ln -fs $(DOTFILES)/alacritty/alacritty.yml			${HOME}/.config/alacritty/alacritty.yml
	@echo Alacritty configuration has been linked. 

fish::
	@test -d ${HOME}/.config || mkdir ${HOME}/.config	${HOME}/.config
	@ln -fns $(DOTFILES)/fish							${HOME}/.config/fish
	@printf "Please run the following to install plugins: \n\n\
	\tfisher install jethrokuan/z \n\
	\tfisher install PatrickF1/fzf.fish\n\n"
	@echo Fish is symlinked.

vim::
	@ln -fs $(DOTFILES)/vim/vimrc					${HOME}/.vimrc
	@echo Vim is symlinked.

neovim::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fns $(DOTFILES)/neovim						${HOME}/.config/nvim
	@echo Neovim is symlinked.

tmux::
	@ln -fs $(tmux)									${HOME}/.tmux.conf
	@ln -fs $(DOTFILES)/tmux/tmux-status.conf		${HOME}/.tmux-status.conf
	@echo tmux is symlinked.

bpswm::
	@echo Window manager has been symlinked.

bspwm::
	[ -d $(BSPWM_DIR) ] || mkdir -p $(BSPWM_DIR)
	@ln -fs $(DOTFILES)/bspwm/Xresources			${HOME}/.Xresources
	@ln -fs $(DOTFILES)/bspwm/xinitrc              	${HOME}/.xinitrc
	@ln -fs $(DOTFILES)/bspwm/bspwmrc              	$(BSPWM_DIR)/bspwmrc
	@echo BSPWM is symlinked.

gtk::
	@ln -fs $(gtk2)									${HOME}/.gtkrc-2.0
	@ln -fs $(gtkrc)								${HOME}/.gtkrc.mine
	@ln -fns $(DOTFILES)/gtk/themes					${HOME}/.themes
	@ln -fs $(DOTFILES)/gtk/user-dirs.dirs			${HOME}/.config/user-dirs.dirs
	@echo GTK is symlinked.
