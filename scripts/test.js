const assert = require("node:assert");
const fs = require("node:fs");
const path = require("node:path");
const childProcess = require("node:child_process");

const projectDir = path.resolve(__dirname, "..");
const macroBuildDir = path.join(projectDir, "macros", ".rescript-macros");
const rescriptCmd =
  process.platform === "win32"
    ? path.join(projectDir, "node_modules", ".bin", "rescript.cmd")
    : path.join(projectDir, "node_modules", ".bin", "rescript");

function runRescript(args) {
  const result = childProcess.spawnSync(rescriptCmd, args, {
    cwd: projectDir,
    encoding: "utf8",
  });

  if (result.status !== 0) {
    throw new Error((result.stdout || "") + (result.stderr || ""));
  }

  return result;
}

function cleanupMacroArtifacts() {
  if (fs.existsSync(macroBuildDir)) {
    fs.rmSync(macroBuildDir, { recursive: true, force: true });
  }
}

cleanupMacroArtifacts();
runRescript(["clean"]);
runRescript(["build"]);

const mainOutputPath = path.join(projectDir, "src", "Main.res.js");
const usesMainOutputPath = path.join(projectDir, "src", "UsesMain.res.js");
const extensionMacroOutputPath = path.join(
  projectDir,
  "macros",
  ".rescript-macros",
  "js",
  "Extension.res.js",
);
const deriverMacroOutputPath = path.join(
  projectDir,
  "macros",
  ".rescript-macros",
  "js",
  "Deriver.res.js",
);
const spiceMacroHelperOutputPath = path.join(
  projectDir,
  "macros",
  ".rescript-macros",
  "js",
  "Spice.res.js",
);

assert.ok(fs.existsSync(mainOutputPath), "Main.res.js should exist");
assert.ok(fs.existsSync(usesMainOutputPath), "UsesMain.res.js should exist");
assert.ok(
  fs.existsSync(extensionMacroOutputPath),
  "compiled extension macro JS should exist",
);
assert.ok(
  fs.existsSync(deriverMacroOutputPath),
  "compiled deriver macro JS should exist",
);
assert.ok(
  fs.existsSync(spiceMacroHelperOutputPath),
  "compiled spice helper JS should exist",
);

const Main = require(mainOutputPath);
const UsesMain = require(usesMainOutputPath);

assert.strictEqual(Main.generatedValue, "macroValue");
assert.strictEqual(Main.message, "query:spice");
assert.strictEqual(Main.userName, "Alice");
assert.strictEqual(Main.getName({ name: "Carla" }), "Carla");

const encodedDrink = Main.encodeDrink({ label: "chai" });
assert.match(JSON.stringify(encodedDrink), /chai/);

const decodedDrink = Main.decodeDrink({ label: "oolong" });
assert.deepStrictEqual(decodedDrink, { label: "oolong" });

assert.strictEqual(UsesMain.forwarded, "macroValue");
assert.strictEqual(UsesMain.fromDeriver, "Bob");
assert.match(UsesMain.fromSpice, /chai/);
assert.match(UsesMain.fromSpice, /oolong/);

console.log("macro poc test passed");
