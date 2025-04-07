set -U fish_greeting

# Starship prompt
starship init fish | source

# atuin
source $HOME/.atuin/bin/env.fish
atuin init fish | source

# path
fish_add_path $HOME/.local/bin

# GPG
set -gx GPG_TTY (tty)

# Aliases
alias estatus="systemctl --user status emacs"
alias estart="systemctl --user start emacs"
alias estop="systemctl --user stop emacs"
alias erestart="systemctl --user restart emacs"

alias et="emacsclient -t"
alias ec="emacsclient -c"

set -gx EDITOR "emacsclient -t"

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

fish_add_path /home/luser/.humanlog/bin

eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

# ASDF configuration code
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims
