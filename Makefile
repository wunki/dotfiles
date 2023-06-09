DOTFILES 	:= $(PWD)
UNAME 		:= $(shell uname -s)

all:: alacritty vim neovim fish tmux zsh zed helix

print-%: ; @echo $*=$($*)

alacritty::
	@test -d ${HOME}/.config/alacritty || mkdir ${HOME}/.config/alacritty	${HOME}/.config/alacritty
	@ln -fs $(DOTFILES)/alacritty/alacritty.yml			${HOME}/.config/alacritty/alacritty.yml
	@echo Alacritty configuration has been linked.

fish::
	@test -d ${HOME}/.config || mkdir ${HOME}/.config	${HOME}/.config
	@ln -fns $(DOTFILES)/fish							${HOME}/.config/fish
	@printf "Please run the following to install plugins: \n\n\
	\tfisher install IlanCosman/tide@v5 \n\
	\tfisher install jethrokuan/z \n\
	\tfisher install PatrickF1/fzf.fish\n\
	\tfisher install jorgebucaran/autopair.fish\n\n"
	@echo Fish is symlinked.

vim::
	@ln -fs $(DOTFILES)/vim/vimrc					${HOME}/.vimrc
	@echo Vim is symlinked.

neovim::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fns $(DOTFILES)/neovim						${HOME}/.config/nvim
	@echo Neovim is symlinked.

zsh::
	@ln -fs $(DOTFILES)/zsh/zprofile				${HOME}/.zprofile
	@ln -fs $(DOTFILES)/zsh/zshrc					${HOME}/.zshrc
	@echo ZSH is symlinked.

zed::
	@ln -fs $(DOTFILES)/zed							${HOME}/.config/zed
	@echo Zed is symlinked.

helix::
	@test -d ${HOME}/.config || mkdir				${HOME}/.config
	@ln -fs $(DOTFILES)/helix/config.toml			${HOME}/.config/helix/config.toml
	@ln -fs $(DOTFILES)/helix/languages.toml		${HOME}/.config/helix/languages.toml
	@echo Helix is symlinked.

tmux::
	@ln -fs $(DOTFILES)/tmux/tmux.conf				${HOME}/.tmux.conf
	@ln -fs $(DOTFILES)/tmux/tmux-status.conf		${HOME}/.tmux-status.conf
	@echo tmux is symlinked.
