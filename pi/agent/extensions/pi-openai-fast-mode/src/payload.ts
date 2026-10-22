import {
  DEFAULT_SERVICE_TIER,
  SUPPORTED_PROVIDERS,
  type FastModeConfig,
  type FastTarget,
  type ModelRef,
} from "./types.ts";

const SUPPORTED_PROVIDER_SET = new Set<string>(SUPPORTED_PROVIDERS);

export function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

export function toModelRef(model: unknown): ModelRef | undefined {
  if (!isRecord(model)) return undefined;

  const { provider, id } = model;
  if (typeof provider !== "string" || typeof id !== "string") return undefined;
  if (!provider || !id) return undefined;

  return { provider, id };
}

export function isSupportedProvider(provider: string): boolean {
  return SUPPORTED_PROVIDER_SET.has(provider);
}

export function findMatchingTarget(
  model: ModelRef | undefined,
  targets: FastTarget[],
): FastTarget | undefined {
  if (!model || !isSupportedProvider(model.provider)) return undefined;

  return targets.find(
    (target) =>
      target.provider === model.provider &&
      target.model === model.id &&
      isSupportedProvider(target.provider),
  );
}

export function applyFastModePayload(
  payload: unknown,
  serviceTier: string,
): unknown | undefined {
  if (!isRecord(payload)) return undefined;

  return {
    ...payload,
    service_tier: serviceTier || DEFAULT_SERVICE_TIER,
  };
}

export function getFastModePayload(
  config: FastModeConfig,
  model: ModelRef | undefined,
  payload: unknown,
): unknown | undefined {
  if (!config.enabled) return undefined;

  const target = findMatchingTarget(model, config.targets);
  if (!target) return undefined;

  return applyFastModePayload(
    payload,
    target.serviceTier ?? DEFAULT_SERVICE_TIER,
  );
}
