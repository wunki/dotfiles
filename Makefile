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
	\tfisher install IlanCosman/tide@v5 \n\
	\tfisher install jethrokuan/z \n\
	\tfisher install PatrickF1/fzf.fish\n"
	\tfisher install jorgebucaran/autopair.fish\n\n"
	@echo Fish is symlinked.

vim::
	@ln -fs $(DOTFILES)/vim/vimrc					${HOME}/.vimrc
	@echo Vim is symlinked.

neovim::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fns $(DOTFILES)/neovim						${HOME}/.config/nvim
	@echo Neovim is symlinked.

helix::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fs $(DOTFILES)/helix/config.toml			${HOME}/.config/helix/config.toml
	@ln -fs $(DOTFILES)/helix/languages.toml		${HOME}/.config/helix/languages.toml
	@echo Helix is symlinked.

tmux::
	@ln -fs $(tmux)									${HOME}/.tmux.conf
	@ln -fs $(DOTFILES)/tmux/tmux-status.conf		${HOME}/.tmux-status.conf
	@echo tmux is symlinked.

bpswm::
	@echo Window manager has been symlinked.

bspwm::
	@[ -d $(BSPWM_DIR) ] || mkdir -p $(BSPWM_DIR)
	@ln -fs $(DOTFILES)/bspwm/Xresources			${HOME}/.Xresources
	@ln -fs $(DOTFILES)/bspwm/xinitrc              	${HOME}/.xinitrc
	@ln -fs $(DOTFILES)/bspwm/bspwmrc              	$(BSPWM_DIR)/bspwmrc
	@ln -fs $(DOTFILES)/bspwm/gtkrc-2.0             ${HOME}/.gtkrc-2.0
	@ln -fns $(DOTFILES)/bspwm/gtk-3.0              ${HOME}/.config/gtk-3.0
	@echo Using Polybar as status bar.
	@ln -fns $(DOTFILES)/bspwm/polybar              ${HOME}/.config/polybar
	@echo Using Dunst for notifications.
	@ln -fns $(DOTFILES)/bspwm/dunst                ${HOME}/.config/dunst
	@echo Using Picom for transparency and shadows.
	@ln -fns $(DOTFILES)/bspwm/picom                ${HOME}/.config/picom
	@echo Using SXHKD for hotkeys.
	@ln -fns $(DOTFILES)/bspwm/sxhkd                ${HOME}/.config/sxhkd
	@echo Using Rofi for hotkeys.
	@ln -fns $(DOTFILES)/bspwm/rofi 				${HOME}/.config/rofi
	@echo Using eww for widgets.
	@ln -fns $(DOTFILES)/bspwm/eww 					${HOME}/.config/eww
	@echo BSPWM is symlinked.

gtk::
	@ln -fs $(gtk2)									${HOME}/.gtkrc-2.0
	@ln -fs $(gtkrc)								${HOME}/.gtkrc.mine
	@ln -fns $(DOTFILES)/gtk/themes					${HOME}/.themes
	@ln -fs $(DOTFILES)/gtk/user-dirs.dirs			${HOME}/.config/user-dirs.dirs
	@echo GTK is symlinked.
