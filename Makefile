DOTFILES	:= $(PWD)
UNAME		:= $(shell uname -s)

all:: fish tmux ghostty

print-%: ; @echo $*=$($*)

fish::
	@test -d ${HOME}/.config || mkdir ${HOME}/.config	${HOME}/.config
	@ln -fns $(DOTFILES)/fish							${HOME}/.config/fish
	@printf "Please run the following to install plugins: \n\n\
	\tfisher install jorgebucaran/hydro \n\
	\tfisher install jorgebucaran/autopair.fish\n\n"
	@echo Fish is symlinked.

zsh::
	@ln -fs $(DOTFILES)/zsh/zprofile				${HOME}/.zprofile
	@ln -fs $(DOTFILES)/zsh/zshrc					${HOME}/.zshrc
	@ln -fs $(DOTFILES)/zsh/zshrc.mac				${HOME}/.zshrc.mac
	@echo ZSH is symlinked.

helix::
	@ln -fs $(DOTFILES)/helix						${HOME}/.config/helix
	@echo Helix is symlinked.

ghostty::
	@ln -fs $(DOTFILES)/ghostty						${HOME}/.config/ghostty
	@echo Ghostty is symlinked.

zed::
	@ln -fs $(DOTFILES)/zed							${HOME}/.config/zed
	@echo Zed is symlinked.

tmux::
	@ln -fs $(DOTFILES)/tmux/tmux.conf				${HOME}/.tmux.conf
	@ln -fs $(DOTFILES)/tmux/tmux-status.conf		${HOME}/.tmux-status.conf
	@echo tmux is symlinked.
