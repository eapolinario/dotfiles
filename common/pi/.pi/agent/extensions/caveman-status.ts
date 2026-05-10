/**
 * Caveman Status Extension
 *
 * Display caveman skill mode in the pi footer status bar.
 *
 * Source of truth (in priority order):
 *   1. Flag file: $CLAUDE_CONFIG_DIR/.caveman-active (or ~/.claude/.caveman-active)
 *      — shared with the Claude Code caveman plugin statusline.
 *   2. Prompt parsing fallback:
 *        /caveman [level]                          → activate
 *        caveman mode | use caveman | talk like..  → activate
 *        stop caveman | normal mode                → deactivate
 *
 * Levels: lite | full | ultra | wenyan-lite | wenyan-full | wenyan-ultra
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";

type Level = "lite" | "full" | "ultra" | "wenyan-lite" | "wenyan-full" | "wenyan-ultra";

const VALID_LEVELS: readonly Level[] = [
	"lite",
	"full",
	"ultra",
	"wenyan-lite",
	"wenyan-full",
	"wenyan-ultra",
] as const;

const STATUS_KEY = "caveman";
const ENTRY_TYPE = "caveman-state";

interface CavemanState {
	active: boolean;
	level: Level;
}

const FLAG_PATH = path.join(
	process.env.CLAUDE_CONFIG_DIR ?? path.join(os.homedir(), ".claude"),
	".caveman-active",
);

function writeFlag(state: CavemanState): void {
	try {
		const dir = path.dirname(FLAG_PATH);
		if (!fs.existsSync(dir)) return; // don't create ~/.claude on non-Claude systems
		// Refuse to overwrite a symlink — same hardening as bash statusline.
		try {
			const lst = fs.lstatSync(FLAG_PATH);
			if (lst.isSymbolicLink()) return;
		} catch {
			/* missing is fine */
		}
		const value = state.active ? state.level : "off";
		fs.writeFileSync(FLAG_PATH, value, { mode: 0o600 });
	} catch {
		/* ignore */
	}
}

function readFlag(): CavemanState | null {
	try {
		const lst = fs.lstatSync(FLAG_PATH);
		// Refuse symlinks — same hardening as bash statusline.
		if (lst.isSymbolicLink()) return null;
		const raw = fs.readFileSync(FLAG_PATH, { encoding: "utf8", flag: "r" }).slice(0, 64);
		const mode = raw
			.replace(/[\r\n]/g, "")
			.toLowerCase()
			.replace(/[^a-z0-9-]/g, "");
		if (!mode || mode === "off") return { active: false, level: "full" };
		if (mode === "wenyan") return { active: true, level: "wenyan-full" };
		if ((VALID_LEVELS as readonly string[]).includes(mode)) {
			return { active: true, level: mode as Level };
		}
		// Other tracked modes (commit/review/compress) — not a level, treat as full.
		return { active: true, level: "full" };
	} catch {
		return null;
	}
}

function parsePrompt(prompt: string): CavemanState | "noop" {
	// `input` event gives us raw user text (no skill block, no template).
	// Trim and treat the whole thing as the command. Single line expected.
	const text = prompt.trim();
	if (!text) return "noop";
	const firstLine = text.split(/\r?\n/, 1)[0].trim();

	if (/^(stop caveman|normal mode)[.!]?$/i.test(firstLine)) {
		return { active: false, level: "full" };
	}

	const slash = firstLine.match(/^\/caveman(?:\s+(\S+))?\s*$/i);
	if (slash) {
		const arg = slash[1]?.toLowerCase();
		const level: Level =
			arg && (VALID_LEVELS as readonly string[]).includes(arg) ? (arg as Level) : "full";
		return { active: true, level };
	}

	// Bare level word as shorthand (e.g. user types just `ultra`).
	const bare = firstLine.toLowerCase();
	if ((VALID_LEVELS as readonly string[]).includes(bare)) {
		return { active: true, level: bare as Level };
	}

	return "noop";
}

function render(state: CavemanState, theme: ExtensionContext["ui"]["theme"]): string | undefined {
	if (!state.active) return undefined;
	const icon = theme.fg("accent", "🦴");
	const label = theme.fg("dim", ` caveman:${state.level}`);
	return icon + label;
}

export default function cavemanStatusExtension(pi: ExtensionAPI): void {
	let state: CavemanState = { active: false, level: "full" };
	let lastCtx: ExtensionContext | null = null;
	let watcher: fs.FSWatcher | null = null;

	function setState(next: CavemanState, persistEntry: boolean): void {
		state = next;
		if (persistEntry) pi.appendEntry(ENTRY_TYPE, state);
		if (lastCtx) lastCtx.ui.setStatus(STATUS_KEY, render(state, lastCtx.ui.theme));
	}

	function refreshFromFlag(persistEntry: boolean): boolean {
		const flag = readFlag();
		if (!flag) return false;
		if (flag.active === state.active && flag.level === state.level) {
			// Still push status in case ctx changed.
			if (lastCtx) lastCtx.ui.setStatus(STATUS_KEY, render(state, lastCtx.ui.theme));
			return true;
		}
		setState(flag, persistEntry);
		return true;
	}

	function startWatcher(): void {
		if (watcher) return;
		try {
			// Watch the directory so we catch create/delete of the flag file too.
			const dir = path.dirname(FLAG_PATH);
			const base = path.basename(FLAG_PATH);
			watcher = fs.watch(dir, (_evt, fname) => {
				if (!fname || fname === base) refreshFromFlag(false);
			});
			watcher.on("error", () => {
				/* ignore */
			});
		} catch {
			/* directory may not exist yet — ignore */
		}
	}

	pi.registerCommand("caveman-status", {
		description: "Show current caveman mode status",
		handler: async (_args, ctx) => {
			lastCtx = ctx;
			refreshFromFlag(false);
			const msg = state.active ? `caveman active: ${state.level}` : "caveman: off";
			ctx.ui.notify(msg, "info");
		},
	});

	pi.on("session_start", async (_event, ctx) => {
		lastCtx = ctx;

		// 1) Flag file wins.
		if (!refreshFromFlag(false)) {
			// 2) Restore prior in-session state.
			const entries = ctx.sessionManager.getEntries();
			const last = entries
				.filter(
					(e: { type: string; customType?: string }) =>
						e.type === "custom" && e.customType === ENTRY_TYPE,
				)
				.pop() as { data?: CavemanState } | undefined;
			if (last?.data) state = last.data;
			ctx.ui.setStatus(STATUS_KEY, render(state, ctx.ui.theme));
		}

		startWatcher();
	});

	pi.on("session_shutdown", async () => {
		if (watcher) {
			try {
				watcher.close();
			} catch {
				/* ignore */
			}
			watcher = null;
		}
	});

	pi.on("input", async (event, ctx) => {
		lastCtx = ctx;
		const result = parsePrompt(event.text ?? "");
		if (result === "noop") return;
		if (!result.active && !state.active) return;
		setState(result, true);
		writeFlag(result);
	});

	pi.on("before_agent_start", async (_event, ctx) => {
		lastCtx = ctx;
		// Pull updates from external writers (Claude Code plugin) if any.
		refreshFromFlag(true);
	});

	pi.on("turn_start", async (_event, ctx) => {
		lastCtx = ctx;
		refreshFromFlag(false);
	});

	pi.on("turn_end", async (_event, ctx) => {
		lastCtx = ctx;
		refreshFromFlag(false);
	});
}
