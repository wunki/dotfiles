# Dotfiles

These are my dotfiles.

## Screenshots

TBD: insert screenshots here.

## Software

| Name        | Description                                                                           |
|-------------|---------------------------------------------------------------------------------------|
| [Alacritty] | Alacritty is a fast terminal.                                                         |
| [Fish]      | Shell with good defaults and easy to configure                                        |
| [Neovim]    | Fast to start, easy to extend. And now configurable with Lua. Must use version 0.5.0+ |
| [Tmux]      | When editing remote I use Tmux to save sessions and manage my windows                 |
| [BPSWM]     | BPSWM as a Window manager. Also using [Polybar] (status bar) and [Rofi] (launcher)    |

[Alacritty]: https://github.com/alacritty/alacritty
[Fish]: https://fishshell.com/
[Neovim]: https://github.com/neovim/neovim
[BSPWM]: https://github.com/baskerville/bspwm
[Polybar]: https://github.com/polybar/polybar
[Rofi]: https://github.com/davatorium/rofi

## Usage

The dotfiles directory contains a `Makefile` which will symlink files into the right place. Proceed with caution though, it will override your own configuration. To give an example, you can run `make neovim` to configure Neovim.


## Commandline tools

I also make use of some convenient command line tools which enrich the default command line tools coming with Unix.

| Name  | Description                                            |
|-------|--------------------------------------------------------|
| [bat] | cat clone with syntax highlighting and Git integration |
| [fd]  | find replacement with better usability                 |
| [exa] | ls replacement with git integration                    |
| [fzf] | command line fuzzy finder                              |
| [btm] | top replacement with pretty graphics                   |

[bat]: https://github.com/sharkdp/bat
[fd]: https://github.com/sharkdp/fd
[exa]: https://github.com/ogham/exa
[fzf]: https://github.com/junegunn/fzf
[btm]: https://github.com/ClementTsang/bottom

