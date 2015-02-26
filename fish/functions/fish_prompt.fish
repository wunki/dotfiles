# git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'git'
set __fish_git_prompt_color_branch yellow

# status chars
set __fish_git_prompt_char_upstream_equal '✓'
set __fish_git_prompt_char_dirtystate '✚'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

# fish prompt
function fish_prompt
  set last_status $status

  # CWD
  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)

  # Git
  set_color normal
  printf '%s ' (__fish_git_prompt)
  set_color normal
end
