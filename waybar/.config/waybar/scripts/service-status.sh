#!/usr/bin/env bash

SERVICE="$1"

case "$2" in
toggle)
    if systemctl --user is-active --quiet "$SERVICE"; then
        systemctl --user stop "$SERVICE"
    else
        systemctl --user start "$SERVICE"
    fi
    ;;
restart)
    systemctl --user restart "$SERVICE"
    ;;
esac

if systemctl --user is-active --quiet "$SERVICE"; then
    echo '{"text":" running","tooltip":"Service is running","class":"running"}'
elif systemctl --user is-failed --quiet "$SERVICE"; then
    echo '{"text":" failed","tooltip":"Service has failed","class":"failed"}'
else
    echo '{"text":" stopped","tooltip":"Service is not running","class":"stopped"}'
fi
