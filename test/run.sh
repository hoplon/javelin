#!/bin/bash

set -e

run_phantom() {
    phantomjs test/runner.js
}

run_ff() {
    this_tty=$(tty)
    xvfb-run -a firefox -profile test/firefox-profile -no-remote "test/test.html" \
    | while read line; do
        if [ "$line" = "Done." ]; then
            ps -t $this_tty | grep firefox | cut -d' ' -f1 | xargs kill -s SIGINT
            echo "All tests passed (on firefox)."
        else
            echo "$line"
        fi
    done
}

run_phantom
run_ff
