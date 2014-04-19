# Automatically start X at boot

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
