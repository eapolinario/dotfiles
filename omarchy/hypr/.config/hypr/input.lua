-- Personal input overrides, loaded after Omarchy's defaults.
-- See https://wiki.hypr.land/Configuring/Basics/Variables/#input

-- Caps Lock is Control, Compose lives on Right Alt.
--
-- Only one xkb option can own Caps Lock. Omarchy 4 defaults to
-- "compose:caps,shift:both_capslock_cancel" (Compose on Caps), and the
-- pre-Quattro config here asked for "compose:caps,ctrl:nocaps" -- where
-- ctrl:nocaps silently won, leaving the machine with Control on Caps and no
-- Compose key at all, so ~/.XCompose never fired. Naming both explicitly keeps
-- Control on Caps and gives ~/.XCompose a working Multi_key.
-- Other homes for Compose: compose:menu, compose:rctrl, compose:rwin.
--
-- shift:both_capslock_cancel is kept from Omarchy's default: with Caps Lock
-- remapped to Control, pressing both Shifts is the only way left to reach an
-- actual Caps Lock. The _cancel variant releases it on the next lone Shift, so
-- an accidental trigger clears itself.
hl.config({
  input = {
    kb_layout = "us",
    kb_options = "ctrl:nocaps,compose:ralt,shift:both_capslock_cancel",

    repeat_rate = 40,
    repeat_delay = 600,

    numlock_by_default = true,

    touchpad = {
      scroll_factor = 0.4,
    },
  },
})

-- Three-finger horizontal swipe changes workspace.
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Gestures/
hl.gesture({ fingers = 3, direction = "horizontal", action = "workspace" })
