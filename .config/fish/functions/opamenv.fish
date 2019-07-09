function opamenv --description "Set the current opam switch in this environment."
    switch (count $argv)
        case 0
            set -l current (opam "switch" show)
            for swch in (opam "switch" list --short)
                if test $swch = $current
                    set_color green
                    printf "* %s\n" $swch
                    set_color normal
                else
                    printf "  %s\n" $swch
                end
            end
        case 1
            eval (opam env --switch $argv --set-switch)
        case '*'
            echo "opam-env [switch]" >&2
    end
end
