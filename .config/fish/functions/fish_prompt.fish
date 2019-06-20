function fish_prompt --description 'Write out the prompt'
  #-----------------------------------------------------------------------------
  # fish_vcs_prompt
  #
  # https://github.com/fish-shell/fish-shell/blob/master/sphinx_doc_src/cmds/fish_vcs_prompt.rst
  # https://github.com/fish-shell/fish-shell/blob/master/sphinx_doc_src/cmds/fish_git_prompt.rst
  # https://mariuszs.github.io/blog/2013/informative_git_prompt.html
  #-----------------------------------------------------------------------------

  # or else git status (not branch) is not shown
  if not set -q __fish_git_prompt_show_informative_status
    set -g __fish_git_prompt_show_informative_status 1
  end

  # hide number of staged (untracked, etc.) files:
  # __fish_git_prompt_hide_stagedstate
  if not set -q __fish_git_prompt_char_stateseparator
    set -g __fish_git_prompt_char_stateseparator ' '
  end

  if not set -q __fish_git_prompt_color_branch
    set -g __fish_git_prompt_color_branch 5EA7EF
  end

  if not set -q __fish_git_prompt_showupstream
    set -g __fish_git_prompt_showupstream 'informative'
  end
  if not set -q __fish_git_prompt_char_upstream_ahead
    set -g __fish_git_prompt_char_upstream_ahead '↑'
  end
  if not set -q __fish_git_prompt_char_upstream_behind
    set -g __fish_git_prompt_char_upstream_behind '↓'
  end
  if not set -q __fish_git_prompt_char_upstream_prefix
    set -g __fish_git_prompt_char_upstream_prefix ' '
  end

  if not set -q __fish_git_prompt_char_stagedstate
    set -g __fish_git_prompt_char_stagedstate '*'
  end
  if not set -q __fish_git_prompt_color_stagedstate
    set -g __fish_git_prompt_color_stagedstate yellow
  end

  if not set -q __fish_git_prompt_char_dirtystate
    set -g __fish_git_prompt_char_dirtystate '+'
  end
  if not set -q __fish_git_prompt_color_dirtystate
    set -g __fish_git_prompt_color_dirtystate 35B63F
  end

  if not set -q __fish_git_prompt_char_untrackedfiles
    set -g __fish_git_prompt_char_untrackedfiles '…'
  end
  if not set -q __fish_git_prompt_color_untrackedfiles
    set -g __fish_git_prompt_color_untrackedfiles F45C8C
  end

  if not set -q __fish_git_prompt_char_invalidstate
    set -g __fish_git_prompt_char_invalidstate '✗'
  end
  if not set -q __fish_git_prompt_color_invalidstate
    set -g __fish_git_prompt_color_invalidstate red
  end

  if not set -q __fish_git_prompt_char_cleanstate
    set -g __fish_git_prompt_char_cleanstate '✓'
  end
  if not set -q __fish_git_prompt_color_cleanstate
    set -g __fish_git_prompt_color_cleanstate green
  end

  #-----------------------------------------------------------------------------
  # color_cwd, prefix and suffix
  #-----------------------------------------------------------------------------

  set -l color_cwd
  set -l prefix
  set -l suffix

  switch "$USER"
    case root toor
      if set -q fish_color_cwd_root
        set color_cwd $fish_color_cwd_root
      else
        set color_cwd $fish_color_cwd
      end

      set suffix '#'
    case '*'
      set color_cwd $fish_color_cwd
      set suffix '$'
  end

  #-----------------------------------------------------------------------------
  # PWD
  #-----------------------------------------------------------------------------

  set_color 1580C6
  echo -n (prompt_pwd)
  echo -n (my_prompt_pwd)
  set_color normal

  #-----------------------------------------------------------------------------
  # final prompt
  #-----------------------------------------------------------------------------

  printf '%s ' (__fish_vcs_prompt)
  echo -n "$suffix "
end
