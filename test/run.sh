#!/bin/bash

set -e

run_phantom() {
    phantomjs --version
    phantomjs test/runner.js
}

run_ff() {
    firefox -version
    this_tty=$(tty)
    xvfb-run -a firefox -profile test/firefox-profile -no-remote "test/test.html" \
    | while read line; do
        if [ "$line" = "Done." ]; then
            kill -n 2 $(ps -t $this_tty | grep firefox | awk '{print $1}')
            echo "All tests passed (on firefox)."
        else
            echo "$line"
        fi
    done
}

run_phantom
run_ff
