#!/bin/bash
# https://stackoverflow.com/a/3980713
stty -echo
printf "Password: "
read PASSWORD
stty echo
echo $PASSWORD
