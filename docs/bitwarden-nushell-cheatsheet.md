# Bitwarden CLI + Nushell Cheatsheet

## Full quick workflow

```nu
$env.BW_SESSION = (bw unlock --raw)
bw sync

let id = (
  bw list items --search github
  | from json
  | first
  | get id
)

bw get item $id | from json
bw get username $id
bw get password $id
bw get totp $id

bw lock
hide-env BW_SESSION
```

Notes:

- Replace `github` with the item search term you want.
- Keep `BW_SESSION` temporary; do not put it in persistent Nushell config.
- Avoid printing passwords unless needed. Prefer piping directly to a clipboard command.
