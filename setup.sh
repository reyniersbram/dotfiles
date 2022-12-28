#! /bin/bash

# The directory of this repository
CDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# clone spicetify config
git clone git@github.com:reyniersbram/spicetify-themes.git ~/.config/spicetify/Themes/

# stow the correct files into the home directory
stow . --ignore 'LICENSE|.*\.md|setup.sh' --target $HOME

