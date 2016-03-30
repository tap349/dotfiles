#!/bin/bash

files=(
  .config/mc
  .config/mpv
  .local/share/mc/skins
  .oh-my-zsh/custom/themes
  .ssh/config
  .vim
  Library/Application\ Support/Karabiner/private.xml
  scripts
  .gitconfig
  .gitignore
  .powconfig
  .pryrc
  .taskrc
  .vimrc
  .zlogin
  .zshenv
  .zshrc
)

# http://misc.flogisoft.com/bash/tip_colors_and_formatting
RED_FG="\033[31m"
LIGHT_GREEN_FG="\033[92m"
RESET_ALL="\033[0m"

for file in "${files[@]}"; do
  echo "symlinking $PWD/$file to $HOME/$file"

  target_dir=`dirname "$file"`
  ln -s "$PWD/$file" "$HOME/$target_dir"

  if [ $? -eq 0 ]; then
    echo -e "[$LIGHT_GREEN_FG OK $RESET_ALL] $file symlinked"
  else
    echo -e "[$RED_FG FAIL $RESET_ALL] $file not symlinked"
  fi

  echo
done

echo SYMLINKING DONE
