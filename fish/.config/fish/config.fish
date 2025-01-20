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
alias ls="ls -gh --color=auto"
alias la="ls -gha --color=auto"

alias estatus="systemctl --user status emacs"
alias estart="systemctl --user start emacs"
alias estop="systemctl --user stop emacs"
alias erestart="systemctl --user restart emacs"

alias ec="emacsclient -c"
alias et="emacsclient -t"
