-- Personal keybinding overrides, loaded after Omarchy's defaults.
-- Inspect the merged result with: omarchy menu keybindings --print
--
-- Anything Omarchy 4 already ships identically (terminal, tmux, browser, file
-- manager, editor, Obsidian, Docker, Spotify, ChatGPT, Grok, YouTube, X) is
-- deliberately absent -- the defaults now cover it.

local home = os.getenv("HOME")

-- Close the active window with SUPER + Q instead of Omarchy's SUPER + W.
hl.unbind("SUPER + W")
hl.bind("SUPER + Q", hl.dsp.window.close(), { description = "Close active window" })

-- Omarchy 4 binds these to preinstalled apps and web apps, so each one has to
-- be released before it can be reused.

-- Was: Email (https://app.hey.com)
hl.unbind("SUPER + SHIFT + E")
o.bind("SUPER + SHIFT + E", "Emacs", { launch = "emacs" })

-- Was: Calendar (https://app.hey.com/calendar/weeks/)
hl.unbind("SUPER + SHIFT + C")
o.bind("SUPER + SHIFT + C", "ChatGPT", { webapp = "https://chatgpt.com/" })

-- Was: Omawrite
hl.unbind("SUPER + SHIFT + W")
o.bind("SUPER + SHIFT + W", "WhatsApp", { webapp = "https://web.whatsapp.com/", focus = true })

-- Was: Passwords (1Password)
hl.unbind("SUPER + SHIFT + SLASH")
o.bind("SUPER + SHIFT + SLASH", "Passwords", { launch = "bitwarden-desktop" })

-- Was: Google Maps (https://maps.google.com/)
hl.unbind("SUPER + SHIFT + S")
o.bind("SUPER + SHIFT + S", "Swap workspace", home .. "/.config/hypr/scripts/swap-workspace.sh")

-- Omarchy 4 moved btop to SUPER + CTRL + T; keep it reachable here too.
o.bind("SUPER + SHIFT + T", "Activity", { tui = "btop" })
