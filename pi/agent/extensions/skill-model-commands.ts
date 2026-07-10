import type { Api, Model } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionCommandContext, ExtensionContext } from "@earendil-works/pi-coding-agent";

type ThinkingLevel = "off" | "minimal" | "low" | "medium" | "high" | "xhigh";

interface SavedState {
	model: Model<Api> | undefined;
	thinkingLevel: ThinkingLevel;
	command: string;
}

const TARGET_PROVIDER = "openai-codex";
const TARGET_MODEL = "gpt-5.5";
const TARGET_THINKING: ThinkingLevel = "medium";

export default function skillModelCommands(pi: ExtensionAPI) {
	let savedState: SavedState | undefined;

	async function switchToSkillModel(command: string, ctx: ExtensionContext): Promise<boolean> {
		if (savedState) {
			ctx.ui.notify(`Already running /${savedState.command} with a temporary model`, "warning");
			return false;
		}

		const target = ctx.modelRegistry.find(TARGET_PROVIDER, TARGET_MODEL);
		if (!target) {
			ctx.ui.notify(`Model not found: ${TARGET_PROVIDER}/${TARGET_MODEL}`, "error");
			return false;
		}

		savedState = {
			model: ctx.model,
			thinkingLevel: pi.getThinkingLevel(),
			command,
		};

		const switched = await pi.setModel(target);
		if (!switched) {
			savedState = undefined;
			ctx.ui.notify(`Could not switch to ${TARGET_PROVIDER}/${TARGET_MODEL}`, "error");
			return false;
		}

		pi.setThinkingLevel(TARGET_THINKING);
		ctx.ui.notify(`/${command}: using ${TARGET_PROVIDER}/${TARGET_MODEL} with ${TARGET_THINKING} thinking`, "info");
		return true;
	}

	async function restoreOriginalModel(ctx: ExtensionContext): Promise<void> {
		if (!savedState) return;

		const previous = savedState;
		savedState = undefined;

		if (previous.model) {
			const restored = await pi.setModel(previous.model);
			if (!restored) {
				ctx.ui.notify(`/${previous.command}: finished, but could not restore the previous model`, "warning");
			}
		}

		pi.setThinkingLevel(previous.thinkingLevel);
		ctx.ui.notify(`/${previous.command}: restored previous model settings`, "info");
	}

	function hasSkillCommand(command: string): boolean {
		return pi.getCommands().some((candidate) => {
			return candidate.source === "skill" && (candidate.name === `skill:${command}` || candidate.name === command);
		});
	}

	async function runSkill(command: "devlog" | "smart-commit", args: string, ctx: ExtensionCommandContext) {
		await ctx.waitForIdle();

		if (!hasSkillCommand(command)) {
			ctx.ui.notify(`Skill not loaded: ${command}`, "error");
			return;
		}

		const switched = await switchToSkillModel(command, ctx);
		if (!switched) return;

		const trimmedArgs = args.trim();
		const skillCommand = trimmedArgs ? `/skill:${command} ${trimmedArgs}` : `/skill:${command}`;
		pi.sendUserMessage(skillCommand);
	}

	pi.registerCommand("devlog", {
		description: `Run the devlog skill with ${TARGET_MODEL} and ${TARGET_THINKING} thinking, then restore the previous model`,
		handler: async (args, ctx) => {
			await runSkill("devlog", args, ctx);
		},
	});

	pi.registerCommand("smart-commit", {
		description: `Run the smart-commit skill with ${TARGET_MODEL} and ${TARGET_THINKING} thinking, then restore the previous model`,
		handler: async (args, ctx) => {
			await runSkill("smart-commit", args, ctx);
		},
	});

	pi.on("agent_end", async (_event, ctx) => {
		await restoreOriginalModel(ctx);
	});
}
