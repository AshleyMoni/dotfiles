#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# feh
feh --bg-center ~/Pictures/Wallpapers/neon-genesis-evangelion-mari-wallpaper-24.jpg

# Compositing
compton -b --config ~/.comptonrc

# activating extra fonts
xset +fp /usr/share/fonts/local
xset fp rehash

# Increase keyboard repeat rate
xset r rate 250 33

# Keyboard
xmodmap ~/.Xmodmap

# Redshift
redshift &

# Load cursor before xmonad
xrdb ~/.Xresources &

# Remove the mouse after lack of use
unclutter -root -grab &

# Run Xmonad
exec xmonad
