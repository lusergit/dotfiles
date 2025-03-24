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

set -gx EDITOR "emacsclient -c"

alias ls="exa -1lxXh --smart-group --git"
alias la="exa -1lxXha --smart-group --git"

export ERL_AFLAGS="-kernel shell_history enabled"

# Haskell
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
set -gx PATH $HOME/.cabal/bin $PATH /home/luser/.ghcup/bin # ghcup-env

# Nim
set -ga fish_user_paths /home/luser/.nimble/bin

# Go
set -x GOPATH $HOME/go
set -x PATH $PATH $GOPATH/bin
set -x GOPATH $HOME/go
set -x PATH $PATH $GOPATH/bin

set -gx COLOR_SCHEME (gsettings get org.gnome.desktop.interface color-scheme)

if [ "$COLOR_SCHEME" = "'default'" ]
    fish_config theme choose modus-operandi
else
    fish_config theme choose modus-vivendi
end
