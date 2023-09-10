function insert_prev_command --description 'insert history command as substitution'
    # copied from fzf_key_bindings.fish
    test -n "$FZF_TMUX_HEIGHT"; or set FZF_TMUX_HEIGHT 40%
    begin
      set -lx FZF_DEFAULT_OPTS "--height $FZF_TMUX_HEIGHT $FZF_DEFAULT_OPTS --scheme=history --bind=ctrl-r:toggle-sort,ctrl-z:ignore $FZF_CTRL_R_OPTS +m"

      history -z | eval (__fzfcmd) --read0 --print0 | read -lz result
      and commandline -i "($result)"
    end
    commandline -f repaint
end
