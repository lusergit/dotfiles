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

fish_config theme choose modus-vivendi
