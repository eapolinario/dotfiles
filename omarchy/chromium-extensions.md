# Chromium extensions

Extensions installed from the Chrome Web Store in the default Chromium profile.
This is a reference list, not something the installer applies: reinstall each
one from its store page.

| Extension | Web Store ID |
| --- | --- |
| [Bitwarden Password Manager](https://chromewebstore.google.com/detail/nngceckbapebfimnlniiiahkandclblb) | `nngceckbapebfimnlniiiahkandclblb` |
| [grasp](https://chromewebstore.google.com/detail/ohhbcfjmnbmgkajljopdjcaokbpgbgfa) | `ohhbcfjmnbmgkajljopdjcaokbpgbgfa` |
| [Send to Kindle for Google Chrome](https://chromewebstore.google.com/detail/cgdjpilhipecahhcilnafpblkieebhea) | `cgdjpilhipecahhcilnafpblkieebhea` |
| [uBlock Origin Lite](https://chromewebstore.google.com/detail/ddkjiahejlhfcafbddmgiahcphecmpfh) | `ddkjiahejlhfcafbddmgiahcphecmpfh` |

grasp sends captures to the `grasp.service` backend in `systemd/`, which
appends them to `~/org/capture.org`.

Omarchy's bundled extensions (copy-url, yt-dlp, whatsapp-slim) are not listed:
Omarchy loads them itself through `--load-extension` in
`~/.config/chromium-flags.conf`.

To refresh this list, open `chromium://extensions` or run:

```bash
ls ~/.config/chromium/Default/Extensions
```
