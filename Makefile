DOTFILES := $(PWD)
all:: vim emacs bin zsh bash tmux xmonad xorg gtk mpv conkeror

vim::
	@ln -fs $(DOTFILES)/vim/vimrc						${HOME}/.vimrc
	@ln -fns $(DOTFILES)/vim							${HOME}/.vim
	@ln -fns $(DOTFILES)/ycm_extra_conf.py				${HOME}/.ycm_extra_conf.py
	@echo Vim is symlinked.

nvim::
	@ln -fs $(DOTFILES)/vim/vimrc						${HOME}/.nvimrc
	@ln -fns $(DOTFILES)/vim							${HOME}/.nvim
	@echo NeoVim is symlinked.

emacs::
	@ln -fs $(DOTFILES)/emacs/emacs.el					${HOME}/.emacs
	@ln -fns $(DOTFILES)/emacs							${HOME}/.emacs.d
	@echo Emacs is symlinked.

bin::
	@ln -fns $(DOTFILES)/bin							${HOME}/bin
	@echo bin files are linked to homedir.

bash::
	@ln -fs $(DOTFILES)/bash/bash_profile			${HOME}/.bash_profile
	@ln -fs $(DOTFILES)/bash/bashrc					${HOME}/.bashrc
	@ln -fs $(DOTFILES)/bash/bash_aliases			${HOME}/.bash_aliases
	@echo Bash is symlinked.

zsh::
	@ln -fs $(DOTFILES)/zsh/zshrc					${HOME}/.zshrc
	@ln -fs $(DOTFILES)/zsh/zshenv					${HOME}/.zshenv
	@ln -fs $(DOTFILES)/zsh/zshalias				${HOME}/.zshalias
	@ln -fs $(DOTFILES)/zsh/pure.zsh 				/usr/local/share/zsh/site-functions/prompt_pure_setup
	@echo ZSH is symlinked.

fish::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fns $(DOTFILES)/fish						${HOME}/.config/fish
	@echo Fish is symlinked.

tarsnap::
	@ln -fs $(DOTFILES)/tarsnap/tarsnapperrc		${HOME}/.tarsnapperrc
	@ln -fs $(DOTFILES)/tarsnap/tarsnaprc			${HOME}/.tarsnaprc
	@echo tarsnap is symlinked.

tmux::
	@ln -fs $(DOTFILES)/tmux/tmux.conf				${HOME}/.tmux.conf
	@ln -fs $(DOTFILES)/tmux/tmux-status.conf		${HOME}/.tmux-status.conf
	@echo tmux is symlinked.

tmuxinator::
	@ln -fns $(DOTFILES)/tmuxinator					${HOME}/.tmuxinator
	@echo tmuxinator is symlinked.

i3::
	@mkdir -p										${HOME}/.i3
	@ln -fs $(DOTFILES)/i3/config					${HOME}/.i3/config
	@ln -fs $(DOTFILES)/i3/i3status.conf			${HOME}/.i3status.conf
	@echo i3 is symlinked.

xmonad::
	@mkdir -p										${HOME}/.xmonad
	@ln -fs $(DOTFILES)/xmonad/xmobarrc				${HOME}/.xmobarrc
	@ln -fns $(DOTFILES)/xmonad/xmonad.hs			${HOME}/.xmonad/xmonad.hs
	@echo XMonad is symlinked.

xorg::
	@ln -fs $(DOTFILES)/xorg/Xdefaults				${HOME}/.Xdefaults
	@ln -fs $(DOTFILES)/xorg/Xresources				${HOME}/.Xresources
	@ln -fs $(DOTFILES)/xorg/xinitrc				${HOME}/.xinitrc
	@ln -fs $(DOTFILES)/xorg/urxvt-perls			${HOME}/.urxvt-perls
	@ln -fns $(DOTFILES)/xorg/fonts.conf			${HOME}/.fonts.conf
	@echo Xorg is symlinked.

gtk::
	@ln -fs $(DOTFILES)/gtk/gtkrc-2.0		${HOME}/.gtkrc-2.0
	@ln -fs $(DOTFILES)/gtk/gtkrc.mine		${HOME}/.gtkrc.mine
	@ln -fns $(DOTFILES)/gtk/themes			  ${HOME}/.themes
	@echo GTK is symlinked.

mpv::
	@mkdir -p ${HOME}/.mpv
	@ln -fns $(DOTFILES)/mpv/config				${HOME}/.mpv/config
	@echo MPV is symlinked.

conkeror::
	@ln -fs $(DOTFILES)/conkeror/conkerorrc.js ${HOME}/.conkerorrc
	@echo Conkeror is symlinked.
