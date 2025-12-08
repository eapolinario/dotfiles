# Authinfo

This directory contains the `.authinfo` file, which is encrypted using [git-crypt](https://github.com/AGWA/git-crypt).

## Decrypting the .authinfo file

The `.authinfo` file is encrypted to protect sensitive credentials. To decrypt it, you need:

1. **git-crypt** installed on your system
2. **Access to the GPG private key** that corresponds to one of the public keys configured in this repository

### Prerequisites

Install git-crypt:

```bash
# On macOS
brew install git-crypt

# On Debian/Ubuntu
sudo apt-get install git-crypt

# On Fedora
sudo dnf install git-crypt
```

### Unlocking the repository

If you have the appropriate GPG private key imported in your keyring, you can unlock the encrypted files:

```bash
# From the repository root
git-crypt unlock
```

This will automatically decrypt all files configured in `.gitattributes`, including `authinfo/.authinfo`.

### Checking encryption status

To see which files are encrypted:

```bash
git-crypt status
```

Files marked as "encrypted" need to be unlocked before you can read their contents.

### Adding new collaborators

If you are the repository owner and want to add a new collaborator who can decrypt these files:

```bash
# Export their GPG public key and add them
git-crypt add-gpg-user GPG_KEY_ID
```

## What is .authinfo?

The `.authinfo` file typically contains authentication credentials in a format used by Emacs and other tools. It usually stores:

- API tokens
- Email credentials
- Other sensitive authentication information

The file format is typically:
```
machine HOSTNAME login USERNAME password PASSWORD
```

## Security Note

Never commit the decrypted `.authinfo` file to the repository. The encryption is automatically handled by git-crypt filters configured in `.gitattributes`.
