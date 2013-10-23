#!/bin/bash

set -e

run_phantom() {
    phantomjs --version
    phantomjs bench/runner.js
}

run_ff() {
    firefox -version
    this_tty=$(tty)
    xvfb-run -a firefox -profile bench/firefox-profile -no-remote "bench/test.html" \
    | while read line; do
        if [ "$line" = "Done." ]; then
            kill -n 2 $(ps -t $this_tty | grep firefox | awk '{print $1}')
            echo "Firefox benchmarks complete."
        else
            echo "$line"
        fi
    done
}

run_phantom
run_ff
