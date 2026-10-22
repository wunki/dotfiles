import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import type {
  ExtensionAPI,
  ExtensionCommandContext,
  ExtensionContext,
  ExtensionFactory,
} from "@earendil-works/pi-coding-agent";
import { getFastCommandCompletions, parseFastCommand } from "./commands.ts";
import {
  cloneConfig,
  loadConfigForScope,
  saveConfigToPath,
  syncSupportedTargets,
} from "./config.ts";
import { getFastModePayload, toModelRef } from "./payload.ts";
import { clearFastStatus, updateFastStatus } from "./status.ts";
import type { FastModeConfig, ModelRef } from "./types.ts";

export type FastModeExtensionOptions = {
  /** Directory containing the extension entry point; used to detect project-local package installs. */
  extensionDir?: string;
  /** Test/advanced override for Pi's user-level agent directory. */
  agentDir?: string;
};

const DEFAULT_EXTENSION_DIR = dirname(fileURLToPath(import.meta.url));

function notifyError(
  ctx: Pick<ExtensionContext, "hasUI" | "ui">,
  error: unknown,
): void {
  if (!ctx.hasUI) return;
  const message = error instanceof Error ? error.message : String(error);
  ctx.ui.notify(message, "error");
}

export function createPiFastModeExtension(
  options: FastModeExtensionOptions = {},
): ExtensionFactory {
  const extensionDir = options.extensionDir ?? DEFAULT_EXTENSION_DIR;
  const agentDir = options.agentDir;

  return function piFastModeExtension(pi: ExtensionAPI): void {
    let config: FastModeConfig = cloneConfig();
    let configPath: string | undefined;
    let loadedCwd: string | undefined;
    let currentModel: ModelRef | undefined;

    async function loadForContext(
      ctx: Pick<ExtensionContext, "cwd">,
    ): Promise<void> {
      const loaded = await loadConfigForScope({
        cwd: ctx.cwd,
        extensionDir,
        agentDir,
      });

      config = syncSupportedTargets(loaded.config);
      configPath = loaded.path;
      loadedCwd = ctx.cwd;

      // Persist the package's current target list on every load so upgrades
      // automatically update existing config files without changing enabled.
      await saveConfigToPath(configPath, config);
    }

    async function ensureLoaded(
      ctx: Pick<ExtensionContext, "cwd">,
    ): Promise<void> {
      if (!configPath || loadedCwd !== ctx.cwd) {
        await loadForContext(ctx);
      }
    }

    async function saveCurrent(
      ctx: Pick<ExtensionContext, "cwd">,
    ): Promise<void> {
      if (!configPath || loadedCwd !== ctx.cwd) {
        await loadForContext(ctx);
      }

      if (!configPath) {
        throw new Error("Fast Mode config path was not resolved");
      }

      await saveConfigToPath(configPath, config);
    }

    function refreshCurrentModel(ctx: Pick<ExtensionContext, "model">): void {
      currentModel = toModelRef(ctx.model) ?? currentModel;
    }

    pi.registerFlag("fast", {
      description: "Start with Fast Mode enabled",
      type: "boolean",
      default: false,
    });

    pi.registerCommand("fast", {
      description: "Toggle Fast Mode. Usage: /fast [on|off|toggle]",
      getArgumentCompletions: getFastCommandCompletions,
      handler: async (
        args: string,
        ctx: ExtensionCommandContext,
      ): Promise<void> => {
        try {
          await ensureLoaded(ctx);
          refreshCurrentModel(ctx);
          config.enabled = parseFastCommand(args, config.enabled);
          await saveCurrent(ctx);
          updateFastStatus(ctx, config, currentModel);
        } catch (error) {
          notifyError(ctx, error);
        }
      },
    });

    pi.on("session_start", async (_event, ctx) => {
      try {
        currentModel = toModelRef(ctx.model);
        await loadForContext(ctx);

        if (pi.getFlag("fast") === true) {
          config.enabled = true;
          await saveCurrent(ctx);
        }

        updateFastStatus(ctx, config, currentModel);
      } catch (error) {
        notifyError(ctx, error);
      }
    });

    pi.on("model_select", async (event, ctx) => {
      currentModel = toModelRef(event.model) ?? toModelRef(ctx.model);
      updateFastStatus(ctx, config, currentModel);
    });

    pi.on("before_provider_request", (event, ctx) => {
      const model = toModelRef(ctx.model) ?? currentModel;
      return getFastModePayload(config, model, event.payload);
    });

    pi.on("session_shutdown", async (_event, ctx) => {
      try {
        if (configPath) {
          await saveConfigToPath(configPath, config);
        }
      } catch (error) {
        notifyError(ctx, error);
      } finally {
        clearFastStatus(ctx);
      }
    });
  };
}

const piFastModeExtension = createPiFastModeExtension();

export default piFastModeExtension;
