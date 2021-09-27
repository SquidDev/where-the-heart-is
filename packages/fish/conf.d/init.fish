# We define a function which converts #RGB -> RGB.
#
# This allows us to use rainbow-mode when editing colours. While set_colour does
# appear to accept the leading `#`, I'm not sure I want to rely on that
# behaviour.
function _hex
  string replace -r "^#" "" $argv
end

# General fish configuration
set fish_greeting     ""
set fish_key_bindings "fish_default_key_bindings"
set fish_emoji_width  2

# Prompt colours
set fish_color_cwd               (_hex "#87d7ff")
set fish_color_dot               (_hex "#af0000")
set fish_color_prompt            (_hex "#5fd75f")
set fish_color_prompt_error      (_hex "#d7005f")

# Syntax colours
set fish_color_normal            "normal"
set fish_color_command           (_hex "#ccd4e3")
set fish_color_autosuggestion    (_hex "#61afef")
set fish_color_param             "normal"
set fish_color_comment           (_hex "#787080")

set fish_color_cancel            -r
set fish_color_error             (_hex "#EC3B86")
set fish_color_history_current   --bold
set fish_color_match             --background=(_hex "#61afef")

set fish_color_quote             (_hex "#98be65")
set fish_color_escape            (_hex "#ddbd78")
set fish_color_valid_path        --underline

set fish_color_end               (_hex "#da8548") # ; and |
set fish_color_redirection       (_hex "#da8548") # > and <
set fish_color_operator          (_hex "#da8548") # * and ~

# set fish_color_search_match      "bryellow" "--background=brblack"
set fish_color_selection         (_hex "#5a5a5a")

set fish_pager_color_prefix      --bold --underline
set fish_pager_color_completion  ""
set fish_pager_color_description (_hex "#ddbd78")
set fish_pager_color_progress    --background=(_hex "#38394c") (_hex "#61afef")

# General git configuation
set __fish_git_prompt_show_informative_status 1
set __fish_git_prompt_showuntrackedfiles      0
set __fish_git_prompt_showupstream            informative

# Git characters
set __fish_git_prompt_char_cleanstate         "✔"
set __fish_git_prompt_char_conflictedstate    "✖"
set __fish_git_prompt_char_dirtystate         "✹"
set __fish_git_prompt_char_stagedstate        "✚"

set __fish_git_prompt_char_stateseparator     " "

set __fish_git_prompt_char_untrackedfiles     "✭"
set __fish_git_prompt_char_upstream_ahead     "↑"
set __fish_git_prompt_char_upstream_behind    "↓"
set __fish_git_prompt_char_upstream_prefix    ""

# Git colours
set __fish_git_prompt_color_cleanstate        (_hex "#87ff00")
set __fish_git_prompt_color_dirtystate        (_hex "#d75f00")
set __fish_git_prompt_color_invalidstate      (_hex "#d70000")
set __fish_git_prompt_color_stagedstate       (_hex "#5fff00")

set __fish_git_prompt_color_branch            (_hex "#61afef")
set __fish_git_prompt_color_untrackedfiles    (_hex "#d7ff00")
set __fish_git_prompt_color_upstream          (_hex "#ff00d7")

set __sq_prompt_opam_color                    (_hex "#f18903")

# Path
set fish_user_paths $HOME/.local/bin $HOME/.local/share/cargo/bin /var/lib/snapd/snap/bin

# Configuration
set -gx EDITOR "vim"
set -gx VISUAL "vim"

# Reexport the manpath. MANPATH=":foo" doesn't work on fish, as the first
# item is dropped.
set -gx MANPATH (manpath -g)

# opam configuration
if test -f $HOME/.opam/opam-init/init.fish
    source $HOME/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
end

# Make the path unique again, so it's a tad cleaner. Ideally we'd use string
# join "\n", but fish doesn't like that.
set -gx PATH (string join0 $PATH | tr '\0' '\n' | awk '!_[$0]++')
