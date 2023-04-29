#!/bin/sh

# Script to adopt uid/gid to host's.
# See https://zenn.dev/anyakichi/articles/73765814e57cba

# Running as root here...

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

# setpriv is a minimal tool like sudo/doas.
# Masquerade to host's user
# Coretto's setpriv does not have --init-groups option. we use --clear-groups.
# Binaries will be deployed into /opt/docker by sbt-native-packager.
exec setpriv --reuid=$USER --regid=$USER --clear-groups /opt/docker/bin/zmm "$@"
