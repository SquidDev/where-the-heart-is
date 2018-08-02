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

    # PWD
    set_color $fish_color_cwd
    echo -n (prompt_pwd)
    set_color normal

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
