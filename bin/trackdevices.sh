#!/bin/bash

# Disable Thinkpad touchpad
# Name obtained using xinput
# xinput disable "SynPS/2 Synaptics TouchPad"

# xinput set-prop "TPPS/2 IBM TrackPoint" "libinput Accel Speed" 0
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 0.5
