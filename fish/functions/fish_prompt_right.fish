function fish_right_prompt
  if test -n "$SSH_CONNECTION"
    printf '%s ' $HOSTNAME
  end
end
