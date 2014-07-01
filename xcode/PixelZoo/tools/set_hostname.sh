#!/bin/sh

if [ $# -ne 1 ]; then
    echo usage: $0 plist-file
    exit 1
fi

plist="$1"
host_name=$(hostname)

/usr/libexec/Plistbuddy -c "Set SERVER_HOST \"$host_name\"" "$plist"
