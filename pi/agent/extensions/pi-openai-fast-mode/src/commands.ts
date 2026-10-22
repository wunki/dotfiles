export const FAST_COMMAND_USAGE = "Usage: /fast [on|off|toggle]";

export class FastCommandUsageError extends Error {
  constructor(message: string = FAST_COMMAND_USAGE) {
    super(message);
    this.name = "FastCommandUsageError";
  }
}

export function parseFastCommand(
  args: string,
  currentEnabled: boolean,
): boolean {
  const normalized = args.trim().toLowerCase();

  if (normalized === "" || normalized === "toggle") {
    return !currentEnabled;
  }

  if (normalized === "on") return true;
  if (normalized === "off") return false;

  throw new FastCommandUsageError();
}

export function getFastCommandCompletions(
  argumentPrefix: string,
): { value: string; label: string }[] {
  const prefix = argumentPrefix.trim().toLowerCase();
  return ["on", "off", "toggle"]
    .filter((option) => option.startsWith(prefix))
    .map((value) => ({ value, label: value }));
}
