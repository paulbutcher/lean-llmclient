// Copyright (c) 2026 Paul Butcher. All rights reserved.
// Released under Apache 2.0 license as described in the file LICENSE.

// Builds Main.lean with Lasm and runs it against a local server standing in for the Claude API.

import { spawn, spawnSync } from "node:child_process";
import { cpSync, mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { createServer } from "node:http";
import { basename, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, "..", "..");

// Lasm writes what it builds into a path dependency's own .lake, where a native build then takes it
// as up to date. So it builds a copy of the working tree, never the tree itself. The copy is kept
// between runs so that what Lasm built last time is reused.
const copy = join(tmpdir(), "llmclient-lasm-test");
mkdirSync(copy, { recursive: true });
const skipped = new Set([".git", ".lake", "node_modules", "dist"]);
cpSync(root, copy, { recursive: true, filter: (src) => !skipped.has(basename(src)) });

const project = join(copy, "test", "lasm");
const build = spawnSync(join(here, "node_modules", ".bin", "lasm"),
  ["build", "Main.lean", "--output", "dist"],
  { cwd: project, stdio: "inherit", env: { ...process.env, LASM_EDITOR: "0" } });
if (build.status !== 0) process.exit(build.status ?? 1);

const claudeText = (text) => ({ content: [{ type: "text", text }], stop_reason: "end_turn" });

function send(res, status, contentType, body) {
  res.writeHead(status, { "content-type": contentType });
  res.end(body);
}

function respond(req, body, res) {
  switch (req.url) {
    case "/echo":
      return send(res, 200, "application/json",
        JSON.stringify(claudeText(JSON.stringify({ headers: req.headers, body }))));
    // Far past any buffer a reader might fill in one go, and multi-byte throughout, so a split
    // that lands inside a character corrupts the text.
    case "/large":
      return send(res, 200, "application/json",
        JSON.stringify(claudeText("é".repeat(1 << 20) + "end")));
    case "/html-error":
      return send(res, 502, "text/html", "<html><body>Bad Gateway</body></html>");
    case "/tool": {
      const results = JSON.parse(body).messages.flatMap((m) =>
        Array.isArray(m.content) ? m.content.filter((b) => b.type === "tool_result") : []);
      if (results.length > 0) {
        return send(res, 200, "application/json",
          JSON.stringify(claudeText(`result:${results[0].content}`)));
      }
      return send(res, 200, "application/json", JSON.stringify({
        content: [{ type: "tool_use", id: "t1", name: "echo_tool", input: { word: "hi" } }],
        stop_reason: "tool_use",
      }));
    }
    default:
      return send(res, 404, "text/plain", "");
  }
}

const listen = (server) =>
  new Promise((resolve) => server.listen(0, "127.0.0.1", () => resolve(server.address().port)));

const server = createServer((req, res) => {
  const chunks = [];
  req.on("data", (c) => chunks.push(c));
  req.on("end", () => respond(req, Buffer.concat(chunks).toString("utf8"), res));
});
const port = await listen(server);

// A port that was just released, so a connection to it is refused.
const closed = createServer();
const deadPort = await listen(closed);
await new Promise((resolve) => closed.close(resolve));

const run = spawn(process.execPath, [join(project, "dist", "main.mjs")], {
  stdio: "inherit",
  env: {
    ...process.env,
    BASE_URL: `http://127.0.0.1:${port}`,
    DEAD_URL: `http://127.0.0.1:${deadPort}/`,
  },
});
const code = await new Promise((resolve) => run.on("exit", (c) => resolve(c ?? 1)));
server.close();
process.exit(code);
