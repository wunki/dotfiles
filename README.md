# Wunki's dotfiles

This are the personal dotfiles of Petar Radosevic ([@wunki]). It's the
accumulation of years of Unix usage. They contain settings for the following
software:

* [Emacs]
* [XMonad]
* [Vim]
* [tmux]
* [Zsh]
* [Mplayer]
* Miscellaneous...

[@wunki]: https://twitter.com/wunki
[Emacs]: http://www.gnu.org/software/emacs/ "Emacs homepage"
[XMonad]: http://xmonad.org/ "XMonad homepage"
[Vim]: http://www.vim.org/ "Vim homepage"
[tmux]: http://tmux.sourceforge.net/ "tmux homepage"
[Zsh]: http://zsh.org/ "Zsh homepage"
[Mplayer]: http://www.mplayerhq.hu/

If you scroll down, you can read a bit more what I use the software for.

## Usage

Clone this repository and update all the submodules.

    git submodule init
    git submodule update

Now you can install the dotfiles in your home directory by using ``make``. For
example type ``make git`` to symlink the git configuration files into your home
directory. 

To install *all* dotfiles, you can run ``make all``.

*Note: If your home directory already contains the configuration files, the
make will not work because the symlinks can't be created*

## Emacs

Emacs is my primary editor and is the software where I spend most of my time
while behind the computer. I'm a heavy user of package.el and have Emacs
configured for Clojure, Haskell, ERC (IRC), mu4e (email) and org-mode.

## XMonad

XMonad is a window manager which I love because I don't have to keep moving
and arranging my windows. It's written and configured in Haskell.

## Vim

Vim is my old editor of choice. Still love the beauty of it's key combinations
but I switched because of the better packages (org-mode, paredit, magit)
available for Emacs.

## TMux

I mainly use tmux when I'm configuring a server. This way I can disconnect
from the server and come back later to see what's happening.

## ZSH

Shell which is I use with [oh-my-zsh] configuration.

## Mplayer

Simple and powerful media player. I have it configured to display subtitles
beautifully and 
