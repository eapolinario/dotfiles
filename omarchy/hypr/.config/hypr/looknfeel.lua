-- Personal look'n'feel overrides, loaded after Omarchy's defaults.

-- https://wiki.hypr.land/Configuring/Basics/Variables/#decoration
hl.config({
  decoration = {
    rounding = 8,
  },
})

-- Workspace names surfaced by the Omarchy bar.
hl.workspace_rule({ workspace = "1", default_name = "emacs" })
hl.workspace_rule({ workspace = "2", default_name = "web", persistent = true })
