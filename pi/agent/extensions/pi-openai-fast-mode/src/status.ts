import { FooterComponent, type ExtensionContext } from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";
import { findMatchingTarget } from "./payload.ts";
import { STATUS_KEY, type FastModeConfig, type ModelRef } from "./types.ts";

type StatusContext = Pick<ExtensionContext, "hasUI" | "mode" | "ui">;
type Indicator = {
  ctx: StatusContext;
  text: string;
  color: "dim" | "success" | "warning";
};

let indicator: Indicator | undefined;
let restoreFooter: (() => void) | undefined;

function installFooter(): void {
  if (restoreFooter) return;
  const original = FooterComponent.prototype.render;
  const render: typeof original = function (this: FooterComponent, width) {
    const lines = original.call(this, width);
    if (!indicator || lines.length < 2 || width < 1) return lines;

    const theme = indicator.ctx.ui.theme;
    const label = theme.fg(indicator.color, indicator.text);
    const suffix = theme.fg("dim", " • ") + label;
    const remaining = width - visibleWidth(suffix);
    if (remaining < 1) {
      lines[1] = truncateToWidth(label, width, "");
    } else {
      // Pi has no inline-footer slot. Decorate its exported renderer so its token,
      // cost, context, directory and other extensions' status logic stay intact.
      // Only the model's row reserves space; other rows keep the full terminal width.
      const stats = original.call(this, remaining)[1] ?? "";
      lines[1] = stats + " ".repeat(Math.max(0, remaining - visibleWidth(stats))) + suffix;
    }
    return lines;
  };
  FooterComponent.prototype.render = render;
  restoreFooter = () => {
    if (FooterComponent.prototype.render === render) FooterComponent.prototype.render = original;
  };
}

export function updateFastStatus(
  ctx: StatusContext,
  config: FastModeConfig,
  model: ModelRef | undefined,
): void {
  if (!ctx.hasUI || ctx.mode !== "tui") return;
  const supported = findMatchingTarget(model, config.targets) !== undefined;
  indicator = {
    ctx,
    text: !config.enabled ? "fast off" : supported ? "fast on" : "fast on (unavailable)",
    color: !config.enabled ? "dim" : supported ? "success" : "warning",
  };
  installFooter();
  // Clear the old standalone widget and ask Pi to repaint after /fast or a model switch.
  ctx.ui.setWidget(STATUS_KEY, undefined);
}

export function clearFastStatus(ctx: StatusContext): void {
  if (!ctx.hasUI || ctx.mode !== "tui") return;
  indicator = undefined;
  restoreFooter?.();
  restoreFooter = undefined;
  ctx.ui.setWidget(STATUS_KEY, undefined);
}
