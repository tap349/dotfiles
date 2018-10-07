#!/bin/bash

echo ==========================================================================
echo SYMLINKING...
echo ==========================================================================

files=(
  .config/mc
  .config/mpv
  .config/yarn
  .gnupg/gpg.conf
  .local/share/mc/skins
  .oh-my-zsh/custom/themes
  .ssh/config
  .vim
  scripts
  colors.tdesktop-palette
  .ctags
  .gitconfig
  .gitignore
  .iex.exs
  .irbrc
  .powconfig
  .pryrc
  .surfingkeys.js
  .zlogin
  .zprofile
  .zshenv
  .zshrc
)

# http://misc.flogisoft.com/bash/tip_colors_and_formatting
# only `echo -e` supports these escape sequences
RED_FG="\033[31m"
LIGHT_GREEN_FG="\033[92m"
LIGHT_BLUE_FG="\033[94m"
RESET_ALL="\033[0m"

for file in "${files[@]}"; do
  echo "symlinking $PWD/$file to $HOME/$file"

  if [ -e "$HOME/$file" ]; then
    read -p "$HOME/$file exists - remove it? [yn] " yn

    if [ -z "$yn" -o ! "$yn" = 'y' ]; then
      echo -e "[$LIGHT_BLUE_FG SKIP $RESET_ALL] symlinking $file skipped"
      echo
      continue
    elif [ "$yn" = 'y' ]; then
      rm -rf "$HOME/$file"
      if [ $? -eq 0 ]; then
        echo -e "[$LIGHT_GREEN_FG OK $RESET_ALL] $file removed"
      else
        echo -e "[$RED_FG FAIL $RESET_ALL] failed to remove $file"
      fi
    fi
  fi

  target_dir=`dirname "$file"`
  ln -s "$PWD/$file" "$HOME/$target_dir"

  if [ $? -eq 0 ]; then
    echo -e "[$LIGHT_GREEN_FG OK $RESET_ALL] $file symlinked"
  else
    echo -e "[$RED_FG FAIL $RESET_ALL] failed to symlink $file"
  fi

  echo
done

echo --------------------------------------------------------------------------
echo SYMLINKING DONE
echo --------------------------------------------------------------------------

echo ==========================================================================
echo COPYING FONTS...
echo ==========================================================================

echo
echo -e "${RED_FG}copy required fonts manually!${RESET_ALL}"
echo
#cp -R Library/Fonts/ ~/Library/Fonts/

echo --------------------------------------------------------------------------
echo FONTS COPIED
echo --------------------------------------------------------------------------
