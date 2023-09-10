if status is-interactive
    # Commands to run in interactive sessions can go here
    fzf_key_bindings
    zoxide init fish | source
    bind [3\;5~ kill-word
    bind \er insert_prev_command
    bind \ee insert_last_command
    bind \ez 'zi; commandline -f repaint'
end
