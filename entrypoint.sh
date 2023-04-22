#!/bin/sh

# Script to adopt uid/gid to host's.
# See https://zenn.dev/anyakichi/articles/73765814e57cba

export USER=zundamon
export HOME=/home/zundamon

uid=$(stat -c "%u" .)
gid=$(stat -c "%g" .)

if [ "$uid" -ne 0 ]; then
    if [ "$(id -g $USER)" -ne $gid ]; then
        # gid of $HOME should be host's
        getent group $gid >/dev/null 2>&1 || groupmod -g $gid $USER
        chgrp -R $gid $HOME
    fi
    if [ "$(id -u $USER)" -ne $uid ]; then
        # uid of $HOME should be host's
        usermod -u $uid $USER
    fi
fi

# Masquerade to host's user
exec setpriv --reuid=$USER --regid=$USER --init-groups "$@"
