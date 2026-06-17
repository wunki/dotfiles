import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { execFile } from "node:child_process";

function runTmux(args: string[], timeoutMs = 10_000): Promise<string> {
  return new Promise((resolve, reject) => {
    execFile("tmux", args, { timeout: timeoutMs, maxBuffer: 1024 * 1024 * 4 }, (error, stdout, stderr) => {
      if (error) {
        const message = stderr.trim() || error.message;
        reject(new Error(message));
        return;
      }

      resolve(stdout);
    });
  });
}

function splitRows(output: string): string[][] {
  return output
    .trim()
    .split("\n")
    .filter(Boolean)
    .map((line) => line.split("\t"));
}

function text(content: unknown) {
  return {
    content: [
      {
        type: "text" as const,
        text: typeof content === "string" ? content : JSON.stringify(content, null, 2),
      },
    ],
    details: typeof content === "object" && content !== null ? (content as Record<string, unknown>) : {},
  };
}

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "tmux_list_sessions",
    label: "tmux: list sessions",
    description: "List currently running tmux sessions.",
    parameters: Type.Object({}),
    async execute() {
      try {
        const output = await runTmux([
          "list-sessions",
          "-F",
          "#{session_name}\t#{session_windows}\t#{session_attached}\t#{session_created}\t#{session_group}",
        ]);

        const sessions = splitRows(output).map(([name, windows, attached, created, group]) => ({
          name,
          windows: Number(windows),
          attached: Number(attached),
          created: Number(created),
          group: group || null,
        }));

        return text({ sessions });
      } catch (error) {
        return text({ sessions: [], note: `No tmux server or unable to list sessions: ${error}` });
      }
    },
  });

  pi.registerTool({
    name: "tmux_list_panes",
    label: "tmux: list panes",
    description: "List tmux panes, including stable pane ids, targets, current commands, and current working directories.",
    parameters: Type.Object({
      target: Type.Optional(
        Type.String({
          description: "Optional tmux target session/window, for example 'my-session' or 'my-session:1'. If omitted, all panes are listed.",
        }),
      ),
    }),
    async execute(_toolCallId, params) {
      const args = [
        "list-panes",
        params.target ? "-s" : "-a",
        "-F",
        "#{session_name}:#{window_index}.#{pane_index}\t#{session_name}\t#{window_index}\t#{window_name}\t#{pane_index}\t#{pane_id}\t#{pane_active}\t#{pane_current_command}\t#{pane_current_path}\t#{pane_title}",
      ];

      if (params.target) args.splice(2, 0, "-t", params.target);

      const output = await runTmux(args);
      const panes = splitRows(output).map(
        ([target, session, windowIndex, windowName, paneIndex, paneId, active, command, path, title]) => ({
          target,
          session,
          window_index: Number(windowIndex),
          window_name: windowName,
          pane_index: Number(paneIndex),
          pane_id: paneId,
          active: active === "1",
          command,
          path,
          title,
        }),
      );

      return text({ panes });
    },
  });

  pi.registerTool({
    name: "tmux_capture_pane",
    label: "tmux: capture pane",
    description: "Read recent output from a tmux pane.",
    parameters: Type.Object({
      target: Type.String({ description: "tmux pane target, for example 'my-session:0.0' or a pane id like '%3'." }),
      start: Type.Optional(
        Type.Integer({ description: "Start line for tmux capture-pane. Use a negative value for recent history. Default: -200." }),
      ),
      end: Type.Optional(Type.Integer({ description: "End line for tmux capture-pane. Default: current visible bottom." })),
      include_ansi: Type.Optional(Type.Boolean({ description: "Include ANSI escape sequences. Default: false." })),
    }),
    async execute(_toolCallId, params) {
      const args = ["capture-pane", "-p", "-t", params.target, "-S", String(params.start ?? -200)];
      if (params.end !== undefined) args.push("-E", String(params.end));
      if (params.include_ansi) args.splice(1, 0, "-e");

      const output = await runTmux(args);
      return text(output.trimEnd());
    },
  });

  pi.registerTool({
    name: "tmux_send_keys",
    label: "tmux: send keys",
    description:
      "Send input to a tmux pane. Prefer 'command' for shell commands. Use 'keys' for tmux key names such as C-c, Up, Enter.",
    parameters: Type.Object({
      target: Type.String({ description: "tmux pane target, for example 'my-session:0.0' or a pane id like '%3'." }),
      command: Type.Optional(Type.String({ description: "Literal text to type into the pane." })),
      enter: Type.Optional(Type.Boolean({ description: "Press Enter after command. Default: true when command is provided." })),
      keys: Type.Optional(Type.Array(Type.String(), { description: "tmux key names to send, for example ['C-c'] or ['Up', 'Enter']." })),
    }),
    async execute(_toolCallId, params) {
      if (!params.command && (!params.keys || params.keys.length === 0)) {
        throw new Error("Provide either command or keys.");
      }

      if (params.command !== undefined) {
        const args = ["send-keys", "-t", params.target, "--", params.command];
        if (params.enter ?? true) args.push("C-m");
        await runTmux(args);
      }

      if (params.keys?.length) {
        await runTmux(["send-keys", "-t", params.target, ...params.keys]);
      }

      return text({ ok: true, target: params.target });
    },
  });

  pi.registerTool({
    name: "tmux_new_session",
    label: "tmux: new session",
    description: "Create a detached tmux session, optionally starting a command in it.",
    parameters: Type.Object({
      name: Type.String({ description: "Session name." }),
      cwd: Type.Optional(Type.String({ description: "Working directory for the new session." })),
      command: Type.Optional(Type.String({ description: "Optional command to run in the new session." })),
    }),
    async execute(_toolCallId, params) {
      const args = ["new-session", "-d", "-s", params.name];
      if (params.cwd) args.push("-c", params.cwd);
      if (params.command) args.push(params.command);

      await runTmux(args);
      return text({ ok: true, session: params.name });
    },
  });
}
