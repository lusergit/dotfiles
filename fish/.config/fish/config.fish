set -U fish_greeting

# Starship prompt
starship init fish | source

# atuin
source $HOME/.atuin/bin/env.fish
atuin init fish | source

# path
fish_add_path $HOME/.local/bin

# asdf
source $HOME/.asdf/asdf.fish

# GPG
set -gx GPG_TTY (tty)

# Aliases
alias estatus="systemctl --user status emacs"
alias estart="systemctl --user start emacs"
alias estop="systemctl --user stop emacs"
alias erestart="systemctl --user restart emacs"

alias et="emacsclient -t"
alias ec="emacsclient -c"

alias ls="exa -1lxXh --smart-group --git"
alias la="exa -1lxXha --smart-group --git"

set -U LIGHT_THEME modus-operandi
set -U DARK_THEME modus-vivendi

set -l gnome_theme (gsettings get org.gnome.desktop.interface color-scheme)

if [ $gnome_theme = "'default'" ]
    set -xU theme_variant light
else
    set -xU theme_variant dark
end

function set_theme
    if [ $theme_variant = light ]
        fish_config theme choose $LIGHT_THEME
    else
        fish_config theme choose $DARK_THEME
    end
end

set_theme
