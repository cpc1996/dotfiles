#!/bin/bash

XkbModel=pc104
XkbLayout=us,us
XkbVariant=colemak,
XkbOptions=caps:ctrl_modifier,lv3:ralt_alt #,terminate:ctrl_alt_bksp,grp:alt_space_toggle

echo "Changing keyboard settings using setxkbmap..."
# This is for the current X session only and ideally should be sourced by .xprofile.
# To also make these options system-wide, run this script manually as root.
setxkbmap -model $XkbModel \
  -layout $XkbLayout \
  -variant $XkbVariant \
  -option $XkbOptions

if [[ $EUID == 0 ]]; then # If is root...
  echo "Changing keyboard settings as root using localectl..."
  # This writes to xorg.conf (usually /etc/X11/xorg.conf.d) and thus is picked up
  # by display manager (requires reboot).
  #
  # Note that if this is both run as root and sourced by .xprofile,
  #   setxkbmap -print -verbose 10
  # will show duplicate XkbOptions.
  #
  # And also note that some of these options could be overriden by other options
  # in xorg.conf and thus have no effect.
  localectl --no-convert set-x11-keymap $XkbLayout $XkbModel $XkbVariant $XkbOptions
fi
