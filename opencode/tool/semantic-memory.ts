import { tool } from "@opencode-ai/plugin";
import { $ } from "bun";

// Rich descriptions that shape agent behavior
// Override via env vars for different contexts
const STORE_DESC =
  process.env.TOOL_STORE_DESCRIPTION ||
  "Persist important discoveries, decisions, and learnings for future sessions. Use for: architectural decisions, debugging breakthroughs, user preferences, project-specific patterns. Include context about WHY something matters.";
const FIND_DESC =
  process.env.TOOL_FIND_DESCRIPTION ||
  "Search your persistent memory for relevant context. Query BEFORE making architectural decisions, when hitting familiar-feeling bugs, or when you need project history. Returns semantically similar memories ranked by relevance.";

async function run(args: string[]): Promise<string> {
  const result = await $`semantic-memory ${args}`.text();
  return result.trim();
}

export const store = tool({
  description: STORE_DESC,
  args: {
    information: tool.schema.string().describe("The information to store"),
    collection: tool.schema
      .string()
      .optional()
      .describe("Collection name (e.g., 'codebase', 'research', 'gotchas')"),
    metadata: tool.schema
      .string()
      .optional()
      .describe("Optional JSON metadata"),
  },
  async execute({ information, collection, metadata }) {
    const args = ["store", information];
    if (collection) args.push("--collection", collection);
    if (metadata) args.push("--metadata", metadata);
    return run(args);
  },
});

export const find = tool({
  description: FIND_DESC,
  args: {
    query: tool.schema.string().describe("Natural language search query"),
    collection: tool.schema
      .string()
      .optional()
      .describe("Collection to search (omit for all)"),
    limit: tool.schema
      .number()
      .optional()
      .describe("Max results (default: 10)"),
  },
  async execute({ query, collection, limit }) {
    const args = ["find", query];
    if (collection) args.push("--collection", collection);
    if (limit) args.push("--limit", String(limit));
    return run(args);
  },
});
