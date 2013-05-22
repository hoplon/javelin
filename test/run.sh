#!/bin/bash

set -e

install_ff_profile() {
    profile_dir=$(find $HOME/.mozilla/ -name '*.default*' -type d)
    echo "user_pref('browser.dom.window.dump.enabled', true);" >> "$profile_dir/user.js"
}

run_phantom() {
    phantomjs test/runner.js
}

run_ff() {
    xvfb-run -a firefox "test/test.html"
}

install_ff_profile
run_phantom
run_ff
