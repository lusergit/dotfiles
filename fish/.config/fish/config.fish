set -U fish_greeting

# Starship prompt
starship init fish | source

# atuin
source $HOME/.atuin/bin/env.fish
atuin init fish | source

# Aliases
alias ls="ls -gh --color=auto"
alias la="ls -gha --color=auto"

alias estatus="systemctl --user status emacs"
alias estart="systemctl --user start emacs"
alias estop="systemctl --user stop emacs"
alias erestart="systemctl --user restart emacs"
