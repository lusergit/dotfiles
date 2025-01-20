set -U fish_greeting

# Starship prompt
starship init fish | source

# atuin
source $HOME/.atuin/bin/env.fish
atuin init fish | source

# asdf
source ~/.asdf/asdf.fish

# Aliases
alias estatus="systemctl --user status emacs"
alias estart="systemctl --user start emacs"
alias estop="systemctl --user stop emacs"
alias erestart="systemctl --user restart emacs"

alias et="emacsclient -t"
alias ec="emacsclient -c"

alias ls="exa -1lxXh --smart-group --git"
alias la="exa -1lxXha --smart-group --git"
