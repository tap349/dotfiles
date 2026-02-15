# For command substitution to work in prompt
setopt prompt_subst

# for i in {0..255}; do print -P "%F{$i}$i%f "; done
PS1='\
%K{#1F2F40}\
%F{032} %1d %f\
$(git_prompt)\
%k\
%F{032} %(!.#.») %f'

git_prompt() {
  local branch=$(git symbolic-ref --short HEAD 2> /dev/null)

  if [[ -z $branch ]]; then
    return 0
  fi

  echo -n %F{75}
  echo -n '('
  echo -n $branch
  echo -n %f

  local has_changes=$(git status --short)

  if [[ -n $has_changes ]]; then
    echo -n %F{214}
    echo -n '*'
    echo -n %f
  fi

  local last_tag=$(git tag --sort=-v:refname | head -n 1 2> /dev/null)

  if [[ -n $last_tag ]]; then
    echo -n %F{230}
    echo -n " $last_tag"
    echo -n %f
  fi

  echo -n %F{75}
  echo -n ') '
  echo -n %f
}
