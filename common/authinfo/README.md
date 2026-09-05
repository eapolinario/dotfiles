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

# On Arch / Omarchy
sudo pacman -S git-crypt
```

### Unlocking the repository

If you have the appropriate GPG private key imported in your keyring, you can unlock the encrypted files:

```bash
# From the repository root
git-crypt unlock
```

This will automatically decrypt all files configured in `.gitattributes`, including `common/authinfo/.authinfo`.

### Checking encryption status

To see which files are encrypted:

```bash
git-crypt status
```

This reports which paths are covered by git-crypt's encryption filters; an
"encrypted" label is not, by itself, proof that the working copy is still locked.

### Omarchy installer policy

The Omarchy installer checks only the fixed git-crypt header, without printing
credentials. A missing, unreadable, or locked authinfo file is skipped with a
warning, leaving an existing `~/.authinfo` untouched. To require credentials:

```sh
./omarchy/install.sh --only authinfo --require-secrets --dry-run
./omarchy/install.sh --only authinfo --require-secrets
```

Unlock separately and rerun if needed; the installer never decrypts the
repository or obtains keys. Replacing an existing regular authinfo file
preserves it in a private `~/.authinfo.backup.*` directory. Treat those backups
as sensitive local files, not repository content.

### Adding new collaborators

If you are the repository owner and want to add a new collaborator who can decrypt these files:

```bash
# Obtain and import their GPG public key, then add them
# GPG_KEY_ID can be the fingerprint or email of an imported key
git-crypt add-gpg-user GPG_KEY_ID
```

## What is .authinfo?

The `.authinfo` file typically contains authentication credentials in a format used by Emacs and other tools. It usually stores:

- API tokens
- Email credentials
- Other sensitive authentication information

The file format (when decrypted) follows the `.netrc` format documented in the netrc(5) manual page:
```
machine example.com login myuser password mypass
```

Common fields include:
- `machine` - the hostname or IP address
- `login` - the username
- `password` - the password

Some tools may support additional fields like `port`, but these are extensions to the standard format.

## Security Notes

### Protection of Encrypted Files

- Never commit the decrypted `.authinfo` file to the repository. The encryption is automatically handled by git-crypt filters configured in `.gitattributes`.
- The encrypted files remain encrypted in the Git history and cannot be decrypted without the appropriate GPG private key.

### Forking This Repository

If someone forks this repository, they can:
- Modify `.gitattributes` to change which files are encrypted
- Add or remove files from the encryption list
- Initialize their own git-crypt setup with their own keys

However, they **cannot**:
- Decrypt the existing `.authinfo` file without the original GPG private key
- Access the symmetric encryption key without the GPG private key that was used to encrypt it

When forking this repository, you would need to:
1. Remove or replace the encrypted `.authinfo` file with your own
2. Set up git-crypt with your own GPG keys
3. Configure your own `.gitattributes` if needed
