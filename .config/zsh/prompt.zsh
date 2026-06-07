# For command substitution to work in prompt
setopt prompt_subst

# for i in {0..255}; do print -P "%F{$i}$i%f "; done
PS1='%K{#1F2F40}%F{032} %1d %f$(git_prompt)%k%F{032} %(!.#.») %f'

git_prompt() {
  if [[ ! -d .git ]]; then
    return 0
  fi

  local branch
  branch=$(git symbolic-ref --short HEAD 2> /dev/null) || return 0

  if [[ -z $branch ]]; then
    return 0
  fi

  local output="%F{75}(${branch}%f"

  local has_changes
  has_changes=$(git status --short)

  if [[ -n $has_changes ]]; then
    output+="%F{214}*%f"
  fi

  local last_tag
  last_tag=$(git tag --sort=-v:refname | head -n 1 2> /dev/null)

  if [[ -n $last_tag ]]; then
    output+="%F{230} ${last_tag}%f"
  fi

  output+="%F{75}) %f"

  printf '%s' "$output"
}
