export const PACKAGE_NAME = "pi-openai-fast-mode";
export const STATUS_KEY = PACKAGE_NAME;
export const DEFAULT_SERVICE_TIER = "priority";

export const SUPPORTED_PROVIDERS = ["openai", "openai-codex"] as const;
export type SupportedProvider = (typeof SUPPORTED_PROVIDERS)[number];

export type FastTarget = {
  provider: string;
  model: string;
  serviceTier?: string;
};

export type FastModeConfig = {
  enabled: boolean;
  targets: FastTarget[];
};

export type ModelRef = {
  provider: string;
  id: string;
};

export type ConfigScope = "user" | "project";

export type ResolvedConfigPath = {
  scope: ConfigScope;
  path: string;
};
