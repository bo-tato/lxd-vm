function insert_last_command --description 'insert last command as substitution'
    commandline -i "($(history | head -n 1))"
end
