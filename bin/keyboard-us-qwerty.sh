#!/bin/bash

XkbModel=pc104
XkbLayout=us
XkbOptions=terminate:ctrl_alt_bksp,caps:ctrl_modifier,lv3:ralt_alt,grp:alt_space_toggle

echo "Changing keyboard settings using setxkbmap..."
setxkbmap -model $XkbModel \
  -layout $XkbLayout \
  -option $XkbOptions
