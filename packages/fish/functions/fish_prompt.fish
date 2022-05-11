function __sq_find_root --description "Finds a directory with a specific file."
    set -l name $argv[1]

    set -l root
    set -l dir (pwd -P)
    while test $dir != "/"
        if test -f $dir'/'$name
            echo $dir
            return 0
        end

        set dir (string replace -r '[^/]*/?$' '' $dir)
    end

    return 1
end

function __sq_opam_prompt --description "Prints the current opam switch"
    if not command -sq opam
        return
    end

    set -l root (__sq_find_root "dune-project")
    or return 0

    set_color $__sq_prompt_opam_color
    printf ' (🐫%s)' (command opam "switch" show)
    set_color normal
end

function __sq_nix --description "Prints if we're in a nix environment"
    if not set -q NIX_CONF_DIR
        return
    end

    set_color $__sq_prompt_opam_color
    printf ' (Nix)'
    set_color normal
end

function fish_prompt --description 'Write out the prompt'
    set -l last_status $status

    if not set -q __fish_prompt_normal
        set -g __fish_prompt_normal (set_color normal)
    end

    set -l color_dot
    set -l prefix
    set -l suffix

    switch "$USER"
        case root toor
            set suffix '•'
        case '*'
            set suffix 'ᐅ'
    end

    # Hostname
    if set -q SSH_CONNECTION
      set_color $__sq_prompt_host_color
      printf '%s ' (hostname)
    end

    # PWD
    set_color $fish_color_cwd
    echo -n (prompt_pwd)
    set_color normal

    printf '%s' (__sq_opam_prompt)
    printf '%s' (__sq_nix)
    printf '%s ' (__fish_vcs_prompt)

    set_color $fish_color_dot
    echo -n "$color_dot"

    if test $last_status -eq 0
        set_color $fish_color_prompt
    else
        set_color $fish_color_prompt_error
    end

    echo -n "$suffix "

    set_color normal
end
