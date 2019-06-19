function fish_prompt --description 'Write out the prompt'
  # or else git status is not shown
  #if not set -q __fish_git_prompt_show_informative_status
  #  set -g __fish_git_prompt_show_informative_status 1
  #end

  #-----------------------------------------------------------------------------
  # git
  #-----------------------------------------------------------------------------

  if not set -q __fish_git_prompt_hide_untrackedfiles
    set -g __fish_git_prompt_hide_untrackedfiles 1
  end

  if not set -q __fish_git_prompt_showupstream
    set -g __fish_git_prompt_showupstream "informative"
  end
  if not set -q __fish_git_prompt_char_upstream_ahead
    set -g __fish_git_prompt_char_upstream_ahead "↑"
  end
  if not set -q __fish_git_prompt_char_upstream_behind
    set -g __fish_git_prompt_char_upstream_behind "↓"
  end
  if not set -q __fish_git_prompt_char_upstream_prefix
    set -g __fish_git_prompt_char_upstream_prefix ""
  end

  if not set -q __fish_git_prompt_char_stagedstate
    set -g __fish_git_prompt_char_stagedstate "*"
  end
  if not set -q __fish_git_prompt_char_dirtystate
    set -g __fish_git_prompt_char_dirtystate "+"
  end
  if not set -q __fish_git_prompt_char_untrackedfiles
    set -g __fish_git_prompt_char_untrackedfiles "…"
  end
  if not set -q __fish_git_prompt_char_invalidstate
    set -g __fish_git_prompt_char_invalidstate "✗"
  end
  if not set -q __fish_git_prompt_char_cleanstate
    set -g __fish_git_prompt_char_cleanstate "✓"
  end

  #-----------------------------------------------------------------------------
  # colors
  #
  # http://fishshell.com/docs/current/commands.html#set_color
  #-----------------------------------------------------------------------------

  if not set -q __fish_git_prompt_color_branch
    set -g __fish_git_prompt_color_branch 5EA7EF
  end

  if not set -q __fish_git_prompt_color_stagedstate
    set -g __fish_git_prompt_color_stagedstate yellow
  end
  if not set -q __fish_git_prompt_color_dirtystate
    set -g __fish_git_prompt_color_dirtystate blue
  end
  if not set -q __fish_git_prompt_color_untrackedfiles
    set -g __fish_git_prompt_color_untrackedfiles brmagenta
  end
  if not set -q __fish_git_prompt_color_invalidstate
    set -g __fish_git_prompt_color_invalidstate red
  end
  if not set -q __fish_git_prompt_color_cleanstate
    set -g __fish_git_prompt_color_cleanstate green
  end

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

  printf '%s ' (__fish_vcs_prompt)

  echo -n "$suffix "
end
