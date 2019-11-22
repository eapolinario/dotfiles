#!/bin/sh

set -eux  # defensive bash programming.

# -f flag removes the target destination before creating the symbolic link. We do this to protect against failures (e.g. existent links).
# -n is to guard against creating a symbolic link inside a directory.
ln -sfn `pwd`/.tmux.conf ~/.tmux.conf
ln -sfn `pwd`/.spacemacs ~/.spacemacs
