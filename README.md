# dotfiles

## Installation

### Linux

```sh
make install-linux
```

### macOS

```sh
make install-macos
```

## Encrypted Files

Some files in this repository (such as `.authinfo`) are encrypted using [git-crypt](https://github.com/AGWA/git-crypt). To work with these files:

1. Install git-crypt on your system
2. Ensure you have the appropriate GPG private key in your keyring
3. Run `git-crypt unlock` from the repository root

For more details, see the [authinfo/README.md](authinfo/README.md).
