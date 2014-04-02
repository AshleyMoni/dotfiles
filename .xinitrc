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

# # Start GNOME Keyring and export controls
# eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

# feh
feh --bg-center ~/Pictures/Wallpapers/neon-genesis-evangelion-mari-wallpaper-24.jpg

# Compositing
compton -b --config ~/.comptonrc

# activating extra fonts
xset +fp /usr/share/fonts/local
xset fp rehash

# Increase keyboard repeat rate
xset r rate 250 33

# Keyboard, activate numlock
xmodmap ~/.Xmodmap
numlockx &

# Redshift
redshift &

# Load cursor before xmonad
xrdb ~/.Xresources &

# Remove the mouse after lack of use
unclutter -root -grab &

# Run Xmonad
exec xmonad
