import { View, div } from "gpui-kit";
import { gridLayout, gridSessions } from "./panopticon.js";

function equal(label, actual, expected) {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`${label}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}
const walk = node => [node, ...(node.children ?? []).flatMap(walk)];

export default class GridChecks extends View {
  render() {
    // Two spaces, only one selected; a settled tab still owns live agents.
    const spaces = [{ active: true, tabs: [{ id: "a" }] }, { tabs: [{ id: "box/a", settled: true }] }];
    const panes = [
      { tab: "a", terminal: "t1", program: { name: "bash" } },
      { tab: "a", terminal: "t2" }, { tab: "a", terminal: null, mod: "files" },
      { tab: "box/a", terminal: "box/t1", program: { agent: true } },
    ];
    const live = (address, extra) => ({ address, state: "live", ...extra });
    const core = [live("t1"), live("t2"), live("box/t1", { parent: "t2" }),
      live("worker", { parent: "box/t1" }), live("helper", { parent: "worker", report: { app: "pi" } }),
      live("unassigned", { agent: "pi" }), { address: "old", state: "exited", parent: "t1", agent: "pi" }];
    const sources = gridSessions(spaces, panes, [{ address: "t2", status: "idle" }], core);
    equal("only agents, in their owning tabs", sources, [
      { address: "t2", tab: "a" }, { address: "box/t1", tab: "box/a" }, { address: "helper", tab: "box/a" },
    ]);
    const tree = gridLayout(sources), nodes = walk(tree);
    equal("one live block per agent", nodes.flatMap(n => n.content?.terminal ? [n.content.terminal] : []),
      ["t2", "box/t1", "helper"]);
    equal("grid not attached to any tab", nodes.some(n => n.content?.tab), false);
    equal("three agents in a 2×2 grid", tree.children.slice(1).map(row => [row.type, row.children.length]),
      [["row", 2], ["row", 2]]);
    equal("no duplicate layout ids", new Set(nodes.map(n => n.id)).size, nodes.length);
    equal("space changes don't filter the grid", gridSessions(spaces.map(s => ({ ...s, active: !s.active })),
      panes, [{ address: "t2" }], core), sources);
    equal("empty state", gridLayout([]).children[1].content.props.role, "empty");
    equal("one full-size tile", gridLayout(sources.slice(0, 1)).children[1].children.length, 1);
    return div().child("Panopticon grid checks passed");
  }
}
