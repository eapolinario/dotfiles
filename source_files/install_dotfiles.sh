#!/bin/sh

set -eux  # defensive bash programming.

# Get absolute path to directory for this script. This is useful if we want to invoke the script from outside the `/source_files` directory.
# I copied the solution from https://gist.github.com/tvlooy/cbfbdb111a4ebad8b93e#gistcomment-1650842.
function abs_script_dir_path {
    SOURCE=${BASH_SOURCE[0]}
    while [ -h "$SOURCE" ]; do
        DIR=$( cd -P $( dirname "$SOURCE") && pwd )
        SOURCE=$(readlink "$SOURCE")
        [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
    done
    DIR=$( cd -P $( dirname "$SOURCE" ) && pwd )
    echo $DIR
}

DIR=$(abs_script_dir_path $0)

# -f flag removes the target destination before creating the symbolic link. We do this to protect against failures (e.g. existent links).
# -n is to guard against creating a symbolic link inside a directory.
ln -sfn $DIR/.tmux.conf ~/.tmux.conf
ln -sfn $DIR/.spacemacs ~/.spacemacs

