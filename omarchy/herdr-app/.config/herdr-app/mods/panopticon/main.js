import { View, div } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import {
  agents, appHome, checking, closeOverlay, layout, layouts, notify, panes, props, readTextFile,
  run, setShared, shared, spaces, terminals, theme, writeTextFile,
} from "herdr";

// One entry file: Herdr confines relative imports to the mod directory, including through Stow links.
const NAME = "panopticon";
const ROOT = "panopticon-agent-grid";
const RETURN_TO = "panopticon.returnLayout";
const statusLabel = { working: "Working", blocked: "Needs you", error: "Needs you", done: "Done", idle: "Idle" };

// Anchor ownership in ALL tabs' panes. Follow live descendants, even through non-agent workers,
// but don't adopt unrelated core terminals. A child with a pane belongs to its own tab.
export function gridSessions(allSpaces, allPanes, allAgents, allTerminals) {
  const tabs = new Set(allSpaces.flatMap(s => s.tabs.map(t => t.id)));
  const reported = new Set(allAgents.map(a => a.address));
  const core = new Map(allTerminals.map(t => [t.address, t]));
  const attached = new Set(allPanes.map(p => p.terminal));
  const children = new Map();
  for (const t of allTerminals) {
    if (!t.parent || t.state !== "live" || attached.has(t.address)) continue;
    if (!children.has(t.parent)) children.set(t.parent, []);
    children.get(t.parent).push(t.address);
  }
  const result = [], seen = new Set();
  function visit(address, tab, pane) {
    const t = core.get(address);
    if (seen.has(address) || (t && t.state !== "live")) return;
    seen.add(address);
    if (reported.has(address) || pane?.program?.agent || pane?.status || t?.program?.agent || t?.report?.app || t?.agent) {
      result.push({ address, tab });
    }
    for (const child of children.get(address) ?? []) visit(child, tab, null);
  }
  for (const p of allPanes) if (p.terminal && tabs.has(p.tab)) visit(p.terminal, p.tab, p);
  return result;
}
const sessions = () => gridSessions(spaces(), panes(), agents(), terminals());
const sameSessions = (a, b) => Array.isArray(a) && a.length === b.length &&
  a.every((s, i) => s.address === b[i].address && s.tab === b[i].tab);

const block = (id, content, extra = {}) => ({ id, type: "block", content, ...extra });
const panel = (role, data = {}) => ({ mod: NAME, props: { role, ...data } });

// These are native terminal blocks, not excerpts read from scrollback. No active-space/tab block.
export function gridLayout(items) {
  const children = [block("panopticon-bar", panel("bar", { items }), { size: { px: 34 } })];
  const columns = Math.ceil(Math.sqrt(items.length));
  for (let i = 0; i < items.length; i += columns) {
    const cells = items.slice(i, i + columns).map(item => {
      const id = `agent-${encodeURIComponent(item.address)}`;
      return { id, type: "column", resizable: false, border: { width: 1, color: "border" }, children: [
        block(`${id}-caption`, panel("caption", item), { size: { px: 26 } }),
        block(`${id}-terminal`, { terminal: item.address }),
      ] };
    });
    // Keep the last row on the same column boundaries as the others.
    if (cells.length < columns) cells.push(block("panopticon-spacer", panel("spacer"), {
      size: { weight: columns - cells.length },
    }));
    children.push({ id: `row-${i}`, type: "row", resizable: false, gap: 4, children: cells });
  }
  if (!items.length) children.push(block("panopticon-empty", panel("empty")));
  return { id: ROOT, type: "column", resizable: false, padding: 4, gap: 4, font_size: 12, children };
}

const owned = tree => tree?.id === ROOT && tree.children?.[0]?.content?.mod === NAME &&
  tree.children[0].content.props?.role === "bar";
function previousLayout() {
  const name = shared(RETURN_TO);
  return name !== NAME && layouts().includes(name) ? name : "main";
}
const back = () => run("layout.switch", { name: previousLayout() });

async function toggle(overlay, cx) {
  const from = shared("layout") || "main";
  if (from === NAME && owned(layout())) { closeOverlay(overlay); back(); return; }
  try {
    // Machine/session IDs are ephemeral. Create only our private runtime layout, never a Stow
    // link or a clone of the user's layout. Compare-and-swap protects concurrent file edits.
    const path = `${appHome()}/layouts/${NAME}.json`;
    const raw = await readTextFile(path);
    const existing = raw === null ? null : JSON.parse(raw);
    if (raw !== null && !owned(existing)) throw new Error(`Leaving the existing ${path} untouched.`);
    const items = sessions();
    if (!existing || !sameSessions(existing.children[0].content.props.items, items)) {
      await writeTextFile(path, JSON.stringify(gridLayout(items), null, 2) + "\n", raw);
    }
    // The app discovers new layout files asynchronously on first use.
    for (let i = 0; i < 30 && !layouts().includes(NAME); i++) await cx.sleep(100);
    if (!layouts().includes(NAME)) throw new Error("Herdr hasn't discovered the grid layout yet. Try again.");
    if ((shared("layout") || "main") !== from) { closeOverlay(overlay); return; }
    setShared(RETURN_TO, from);
    closeOverlay(overlay);
    run("layout.switch", { name: NAME });
  } catch (error) {
    closeOverlay(overlay);
    notify({ title: "Couldn't open Panopticon", body: String(error), level: "error" });
  }
}

function caption({ address, tab }, t) {
  const a = agents().find(a => a.address === address);
  const p = a ? null : panes().find(p => p.terminal === address);
  const term = a ? null : terminals().find(k => k.address === address);
  const space = a ? null : spaces().find(s => s.tabs.some(c => c.id === tab));
  const location = [a?.space_name || space?.name, a?.tab_name || space?.tabs.find(c => c.id === tab)?.name];
  const program = (a || p || term)?.program?.name || term?.name || "Agent";
  const status = a?.status || p?.status || term?.status;
  const pane = a?.pane || p?.pane;
  return h_flex().id("caption").size_full().px(7).gap(6).items_center().bg(t.chrome)
    .text_size(12).cursor_pointer().hover(el => el.bg(t.hover))
    .tooltip(`${a?.title || p?.title || program} · ${address} — ${pane ? "Go to original pane" : "Peek at agent"}`)
    .on_click(() => {
      back();
      if (pane) run("pane.focus", { pane });
      else { run("tab.select", { id: tab }); run("terminal.peek", { terminal: address }); }
    })
    .child(div().flex_1().min_w(0).truncate().text_color(t.foreground)
      .child(`${program} · ${location.filter(Boolean).join(" / ")} · ${address}`))
    .child(div().flex_none().text_color(t[status === "error" ? "blocked" : status] || t.dim)
      .child(statusLabel[status] || "Open"));
}

export default class Panopticon extends View {
  init(_props, cx) {
    const { overlay, role } = props() ?? {};
    if (overlay && !role && !checking()) cx.spawn(cx => toggle(overlay, cx));
  }

  render() {
    const p = props() ?? {}, t = theme();
    if (p.role === "caption") return caption(p, t);
    if (p.role === "spacer") return div();
    if (p.role === "bar") {
      const items = sessions();
      // Only membership/location changes rebuild the grid. Output paints natively, and status
      // changes update captions without moving tiles or repeatedly resizing running terminals.
      if (!checking() && shared("layout") === NAME && owned(layout()) && !sameSessions(p.items, items)) {
        const key = JSON.stringify(items);
        if (key !== this.pending) { this.pending = key; run("layout.set", { layout: gridLayout(items) }); }
      } else this.pending = null;
      return h_flex().size_full().px(8).gap(12).items_center().bg(t.chrome).text_size(13)
        .child(div().font_semibold().text_color(t.foreground).child("Panopticon"))
        .child(div().flex_1().text_color(t.muted).child(`All spaces · ${items.length} agent${items.length === 1 ? "" : "s"}`))
        .child(div().id("back").px(9).py(3).rounded(5).bg(t.surface).text_color(t.foreground)
          .cursor_pointer().hover(el => el.bg(t.hover)).on_click(back).child(`Back to ${previousLayout()}  ·  Ctrl+Shift+G`));
    }
    return v_flex().size_full().items_center().justify_center().text_color(t.muted)
      .child(p.role === "empty" ? "No open agent sessions across your spaces." : "Switching layouts…");
  }
}
