import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { existsSync, realpathSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

// Use the same host modules as the installed Pi, without installing a second SDK.
let host = dirname(realpathSync(execFileSync("which", ["pi"], { encoding: "utf8" }).trim()));
while (!existsSync(join(host, "package.json"))) {
  assert.notEqual(host, dirname(host), "could not locate the installed Pi package");
  host = dirname(host);
}
const sdkPath = join(host, "dist/index.js");
const { createAgentSession, DefaultResourceLoader, FooterComponent, ModelRuntime, SessionManager, SettingsManager, initTheme } = await import(sdkPath);
const { theme } = await import(join(dirname(sdkPath), "modes/interactive/theme/theme.js"));
const { visibleWidth } = await import(join(host, "node_modules/@earendil-works/pi-tui/dist/index.js"));
const { getModel } = await import(join(host, "node_modules/@earendil-works/pi-ai/dist/compat.js"));
const source = fileURLToPath(new URL("../src/index.ts", import.meta.url));
const plain = (text) => text.replace(/\x1b\[[0-9;]*m/g, "");

initTheme("dark", false);

for (const provider of ["openai", "openai-codex"]) {
  test(`${provider}/gpt-6.1-sol requests priority with a live inline toggle`, async () => {
    const cwd = await mkdtemp(join(tmpdir(), "pi-fast-footer-test-"));
    const configPath = join(cwd, "extensions", "pi-openai-fast-mode", "config.json");
    const original = FooterComponent.prototype.render;
    const previousAgentDir = process.env.PI_CODING_AGENT_DIR;
    process.env.PI_CODING_AGENT_DIR = cwd;
    let session;
    try {
      // An existing config must gain the new target without changing its saved toggle.
      await mkdir(dirname(configPath), { recursive: true });
      await writeFile(configPath, JSON.stringify({ enabled: false, targets: [
        { provider, model: "gpt-5.5", serviceTier: "priority" },
      ] }));
      const settingsManager = SettingsManager.inMemory({ packages: [] });
      const loader = new DefaultResourceLoader({
        cwd, agentDir: cwd, settingsManager,
        additionalExtensionPaths: [source],
        noSkills: true, noPromptTemplates: true, noThemes: true,
        agentsFilesOverride: () => ({ agentsFiles: [] }),
      });
      await loader.reload();
      assert.deepEqual(loader.getExtensions().errors, []);
      const modelRuntime = await ModelRuntime.create({ authPath: join(cwd, "auth.json"), modelsPath: join(cwd, "models.json") });
      await modelRuntime.setRuntimeApiKey("openai", "test-only-no-network-requests");
      const model = getModel(provider, "gpt-6.1-sol");
      assert.ok(model, "the installed host must include the current model");
      ({ session } = await createAgentSession({
        cwd, agentDir: cwd, settingsManager, resourceLoader: loader, modelRuntime,
        model, thinkingLevel: "high",
        sessionManager: SessionManager.inMemory(cwd), tools: [],
      }));
      const footer = new FooterComponent(session, {
        getGitBranch: () => "footer-test",
        getAvailableProviderCount: () => 2,
        getExtensionStatuses: () => new Map([["other-extension", "other status"]]),
      });
      const baseline = footer.render(160);
      const notifications = [];
      await session.bindExtensions({ mode: "tui", uiContext: {
        theme,
        setWidget: (_key, value) => assert.equal(value, undefined, "no extra widget row"),
        notify: (message) => notifications.push(message),
      } });
      const errors = [];
      session.extensionRunner.onError((error) => errors.push(error));
      assert.match(plain(footer.render(160)[1]), /gpt-6\.1-sol • high • fast off$/);
      assert.deepEqual(await session.extensionRunner.emitBeforeProviderRequest({ model: model.id }), { model: model.id });

      await session.prompt("/fast on");
      assert.match(plain(footer.render(160)[1]), /gpt-6\.1-sol • high • fast on$/);
      assert.deepEqual(await session.extensionRunner.emitBeforeProviderRequest({ model: model.id }), {
        model: model.id, service_tier: "priority",
      });
      assert.equal(footer.render(160)[0], baseline[0], "directory row is unchanged");
      assert.equal(footer.render(160)[2], baseline[2], "other extension statuses are unchanged");
      for (const width of [1, 8, 20, 40, 80, 160]) {
        for (const line of footer.render(width)) assert.ok(visibleWidth(line) <= width);
      }

      // Another session's saved toggle must not misrepresent this session's live state.
      const saved = JSON.parse(await readFile(configPath, "utf8"));
      await writeFile(configPath, JSON.stringify({ ...saved, enabled: false }));
      assert.match(plain(footer.render(160)[1]), /fast on$/);

      await session.prompt("/fast off");
      assert.match(plain(footer.render(160)[1]), /gpt-6\.1-sol • high • fast off$/);
      assert.deepEqual(await session.extensionRunner.emitBeforeProviderRequest({ model: model.id }), { model: model.id });
      await session.prompt("/fast");
      assert.match(plain(footer.render(160)[1]), /gpt-6\.1-sol • high • fast on$/);
      assert.deepEqual(await session.extensionRunner.emitBeforeProviderRequest({ model: model.id }), {
        model: model.id, service_tier: "priority",
      });

      const openaiModel = getModel("openai", "gpt-6.1-sol");
      assert.ok(openaiModel);
      await session.setModel({ ...openaiModel, id: "gpt-5.5" });
      assert.match(plain(footer.render(160)[1]), /gpt-5\.5 • high • fast on$/);
      assert.deepEqual(await session.extensionRunner.emitBeforeProviderRequest({ model: "gpt-5.5" }), {
        model: "gpt-5.5", service_tier: "priority",
      });
      await session.setModel({ ...openaiModel, id: "unlisted-model" });
      assert.match(plain(footer.render(160)[1]), /unlisted-model • high • fast on \(unavailable\)$/);
      assert.deepEqual(await session.extensionRunner.emitBeforeProviderRequest({ model: "unlisted-model" }), { model: "unlisted-model" });
      await session.prompt("/fast invalid");
      assert.match(notifications.pop(), /Usage: \/fast/);
      assert.match(plain(footer.render(160)[1]), /fast on \(unavailable\)$/);
      assert.deepEqual(errors, []);
      await session.extensionRunner.emit({ type: "session_shutdown", reason: "exit" });
      assert.equal(FooterComponent.prototype.render, original, "shutdown restores Pi's renderer");
    } finally {
      if (session) {
        await session.extensionRunner.emit({ type: "session_shutdown", reason: "exit" });
        session.dispose();
      }
      if (previousAgentDir === undefined) delete process.env.PI_CODING_AGENT_DIR;
      else process.env.PI_CODING_AGENT_DIR = previousAgentDir;
      await rm(cwd, { recursive: true, force: true });
    }
  });
}
