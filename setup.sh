#! /bin/bash

# The directory of this repository
CDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Download themes for Spicetify
$CDIR/setup/spicetify_themes.sh

mkdir -p $HOME/.config/
# stow the correct files into the home directory
stow . -v --dir $CDIR --target $HOME

