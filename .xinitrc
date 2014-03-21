#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Set background and rotate it
fehbg &

# Start compositing with Compton
compton -fb -D 5

# Redshift
redshift -t 6500:5500 &

# Increase keyboard repeat rate
xset r rate 250 33


# Run Xmonad
exec xmonad
