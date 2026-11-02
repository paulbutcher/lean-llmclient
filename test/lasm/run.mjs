// Copyright (c) 2026 Paul Butcher. All rights reserved.
// Released under Apache 2.0 license as described in the file LICENSE.

// Builds the tests in Tests.lean with Lasm and runs them against a local server standing in for
// the Claude API: on Node through Main.lean, then in headless Chromium through Browser.lean.

import { spawn, spawnSync } from "node:child_process";
import { cpSync, mkdirSync, readFileSync } from "node:fs";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { basename, dirname, extname, join, normalize } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright";

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, "..", "..");

// Lasm writes what it builds into a path dependency's own .lake, where a native build then takes it
// as up to date. So it builds a copy of the working tree, never the tree itself. The copy is kept
// between runs so that what Lasm built last time is reused.
const copy = join(tmpdir(), "llmclient-lasm-test");
mkdirSync(copy, { recursive: true });
const skipped = new Set([".git", ".lake", "node_modules", "dist", "dist-browser"]);
cpSync(root, copy, { recursive: true, filter: (src) => !skipped.has(basename(src)) });

const project = join(copy, "test", "lasm");
function lasmBuild(...args) {
  const build = spawnSync(join(here, "node_modules", ".bin", "lasm"), ["build", ...args],
    { cwd: project, stdio: "inherit", env: { ...process.env, LASM_EDITOR: "0" } });
  if (build.status !== 0) process.exit(build.status ?? 1);
}
lasmBuild("Main.lean", "--output", "dist");
lasmBuild("Browser.lean", "--target", "browser", "--output", "dist-browser");

const claudeText = (text) => ({ content: [{ type: "text", text }], stop_reason: "end_turn" });

// The page is served from a different origin, so every route but /no-cors allows it in, as the
// real APIs do once asked properly.
function send(req, res, status, contentType, body) {
  const cors = req.url === "/no-cors" ? {} : { "access-control-allow-origin": "*" };
  res.writeHead(status, { "content-type": contentType, ...cors });
  res.end(body);
}

function preflight(req, res) {
  if (req.url === "/no-cors") {
    res.writeHead(204);
  } else {
    res.writeHead(204, {
      "access-control-allow-origin": "*",
      "access-control-allow-methods": "POST",
      "access-control-allow-headers": req.headers["access-control-request-headers"] ?? "",
    });
  }
  res.end();
}

function respond(req, body, res) {
  const json = (value) => send(req, res, 200, "application/json", JSON.stringify(value));
  switch (req.url) {
    case "/echo":
      return json(claudeText(JSON.stringify({ headers: req.headers, body })));
    // Far past any buffer a reader might fill in one go, and multi-byte throughout, so a split
    // that lands inside a character corrupts the text.
    case "/large":
      return json(claudeText("é".repeat(1 << 20) + "end"));
    case "/html-error":
      return send(req, res, 502, "text/html", "<html><body>Bad Gateway</body></html>");
    case "/no-cors":
      return json(claudeText("the page should never see this"));
    case "/tool": {
      const results = JSON.parse(body).messages.flatMap((m) =>
        Array.isArray(m.content) ? m.content.filter((b) => b.type === "tool_result") : []);
      if (results.length > 0) return json(claudeText(`result:${results[0].content}`));
      return json({
        content: [{ type: "tool_use", id: "t1", name: "echo_tool", input: { word: "hi" } }],
        stop_reason: "tool_use",
      });
    }
    default:
      return send(req, res, 404, "text/plain", "");
  }
}

const listen = (server) =>
  new Promise((resolve) => server.listen(0, "127.0.0.1", () => resolve(server.address().port)));

const api = createServer((req, res) => {
  if (req.method === "OPTIONS") return preflight(req, res);
  const chunks = [];
  req.on("data", (c) => chunks.push(c));
  req.on("end", () => respond(req, Buffer.concat(chunks).toString("utf8"), res));
});
const base = `http://127.0.0.1:${await listen(api)}`;

// A port that was just released, so a connection to it is refused.
const closed = createServer();
const deadPort = await listen(closed);
await new Promise((resolve) => closed.close(resolve));
const dead = `http://127.0.0.1:${deadPort}/`;

async function onNode() {
  const run = spawn(process.execPath, [join(project, "dist", "main.mjs")],
    { stdio: "inherit", env: { ...process.env, BASE_URL: base, DEAD_URL: dead } });
  return await new Promise((resolve) => run.on("exit", (code) => resolve(code === 0)));
}

const types = { ".html": "text/html", ".js": "text/javascript", ".wasm": "application/wasm" };
const site = join(project, "dist-browser");
const flags = JSON.stringify({ base, dead }).replaceAll("'", "&#39;");
const pages = createServer((req, res) => {
  const path = req.url === "/" ? "/index.html" : req.url;
  const file = normalize(join(site, path));
  try {
    let body = readFileSync(file);
    if (path === "/index.html") {
      body = body.toString().replace("data-lasm-app", `data-lasm-app data-lasm-flags='${flags}'`);
    }
    res.writeHead(200, { "content-type": types[extname(file)] ?? "application/octet-stream" });
    res.end(body);
  } catch {
    res.writeHead(404);
    res.end();
  }
});
// Not the API's address: 127.0.0.1 and localhost are different origins to a browser.
const page = `http://localhost:${await listen(pages)}/`;

async function inBrowser() {
  const browser = await chromium.launch();
  try {
    const tab = await browser.newPage();
    const lines = [];
    const finished = new Promise((resolve) => {
      const finish = (passed) => setTimeout(() => resolve(passed), 500);
      tab.on("console", (message) => {
        const text = message.text();
        lines.push(text);
        if (text === "All Lasm tests passed.") finish(true);
        else if (text.includes("Lasm test(s) failed") || text.startsWith("Lasm.Web.background:")) {
          finish(false);
        }
      });
      tab.on("pageerror", (error) => {
        lines.push(`page error: ${error.message}`);
        finish(false);
      });
    });
    await tab.goto(page);
    const timeout = new Promise((resolve) => setTimeout(() => {
      lines.push("timed out waiting for the tests to report");
      resolve(false);
    }, 60_000));
    const passed = await Promise.race([finished, timeout]);
    // The console also carries the browser's own CORS and network messages, which are expected
    // here; on failure they are the evidence.
    if (passed) console.log("All Lasm tests passed in Chromium.");
    else console.log(["Lasm tests failed in Chromium:", ...lines].join("\n"));
    return passed;
  } finally {
    await browser.close();
  }
}

const passedOnNode = await onNode();
const passedInBrowser = await inBrowser();
api.close();
pages.close();
process.exit(passedOnNode && passedInBrowser ? 0 : 1);
