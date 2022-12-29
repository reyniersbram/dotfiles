#!/bin/bash

test -d "$HOME/.config/spicetify/Themes/"
themes_exist=$((!$?))

if [[ $themes_exist -gt 0 ]]; then
    echo "$HOME/.config/spicetify/Themes/ already exists."
    read -r -n 1 -p "Overwrite these themes? [y/N]: "
    echo
    case $REPLY in
        y|Y )   echo "Removing $HOME/.config/spicetify/Themes..."
                rm -rf "$HOME/.config/spicetify/Themes"
                overwrite_themes=1
                ;;
        *   )   echo "Not updating Spicetify themes."
                ;;
    esac
fi

if [[ $themes_exist -eq 0 || $overwrite_themes -eq 1 ]]; then
    # clone spicetify config
    git clone git@github.com:reyniersbram/spicetify-themes.git ~/.config/spicetify/Themes/
fi

