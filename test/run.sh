#!/bin/bash

set -e

PJ=0
FF=0

run_phantom() {
    phantomjs --version
    phantomjs runner.js
    PJ=$?
}

run_ff() {
    firefox -version
    this_tty=$(tty)
    xvfb-run -a firefox -profile firefox-profile -no-remote "test.html" \
    | while read -t 5 line; do
        if [ "$line" = "Done." ]; then
            kill -n 2 $(ps -t $this_tty | grep firefox | awk '{print $1}')
            echo "All tests passed (on firefox)."
            FF=0
        else
            echo "$line"
            FF=1
        fi
    done
}

run_phantom
#run_ff

[ "$FF" == "0" ] && [ "$PJ" == "0" ] && exit 0
exit 1
