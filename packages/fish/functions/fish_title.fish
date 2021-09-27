function fish_title --description 'Write out the title'
    printf 'terminal: %s (%s)' (status current-command) (pwd)
end
