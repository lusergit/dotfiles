set -U fish_greeting

# Source completions only if interactive
if status is-interactive
    # Starship prompt
    starship init fish | source

    # atuin
    source $HOME/.atuin/bin/env.fish
    atuin init fish | source

    # # JJ
    # COMPLETE=fish jj | source
end

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

alias ls="eza -1lxXh --smart-group --git"
alias la="eza -1lxXha --smart-group --git"

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

function open_pr
    set change "$argv[1]"
    jj git push -c "$change"
    set bookmark "$(jj show "$change" --template 'remote_bookmarks' --no-patch | sd '\s+' '\n' | rg '^push-' | head -n 1 | sd '\@.*' '')"
    set description "$(jj show "$change" --template 'description' --no-patch)"
    set header "$(echo "$description" | head -n 1)"
    set body "$(echo "$description" | tail -n +2 | rg -v '^[\w\-]+:\s' | rg --multiline --multiline-dotall '\s*([^\s].*[^\s]\n)\s*' -r '$1')"
    gh pr create --base "$argv[2]" --body "$body" --title "$header" --head "lusergit:$bookmark"
end

# jekyll
set -gx GEM_HOME "$HOME/.gems"
fish_add_path "$HOME/.gems/bin"

# brew
if test -d /home/linuxbrew/.linuxbrew
    # Homebrew is installed on Linux

    set -gx HOMEBREW_PREFIX "/home/linuxbrew/.linuxbrew"
    set -gx HOMEBREW_CELLAR "/home/linuxbrew/.linuxbrew/Cellar"
    set -gx HOMEBREW_REPOSITORY "/home/linuxbrew/.linuxbrew/Homebrew"
    set -gx PATH "/home/linuxbrew/.linuxbrew/bin" "/home/linuxbrew/.linuxbrew/sbin" $PATH
    set -q MANPATH; or set MANPATH ''
    set -gx MANPATH "/home/linuxbrew/.linuxbrew/share/man" $MANPATH
    set -q INFOPATH; or set INFOPATH ''
    set -gx INFOPATH "/home/linuxbrew/.linuxbrew/share/info" $INFOPATH

    # Homebrew asked for this in order to `brew upgrade`
    set -gx HOMEBREW_GITHUB_API_TOKEN {api token goes here, don't remember where that's created}
else if test -d /opt/homebrew
    # Homebrew is installed on MacOS

    /opt/homebrew/bin/brew shellenv | source
end
