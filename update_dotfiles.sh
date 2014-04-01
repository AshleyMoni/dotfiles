#!/bin/bash

# This script is intended simply for maintenance on the dotfiles repository; it
# automatically copies in new versions of any relevant files.

## shell files
cp ~/.bashrc .
cp ~/.zshrc .
cp ~/.zshenv .

## rc files
cp ~/.emacs .
cp ~/.vimperatorrc .
cp ~/.comptonrc .
cp ~/.config/redshift.conf .
cp ~/.gitignore .

## Xmonad, dzen, conky, etc.
cp -r ~/.xmonad .

# X files
cp ~/.Xmodmap .
cp ~/.xinitrc .
cp ~/.Xdefaults .
