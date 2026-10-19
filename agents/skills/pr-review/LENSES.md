# Review lenses

Checklists and smell-to-fix examples for each lens in SKILL.md. Examples use TypeScript, but the ideas carry to any language.

## 1. Correctness and failure paths

- **Inputs:** what happens with empty, null, unknown, duplicate, very large or malformed input? Is untrusted data (disk, network, IPC, user, model output) parsed once at the edge, or trusted deep inside?
- **Async:** can two operations interleave? Does a late result from an old request, account or page overwrite newer state? Is there a cancellation or "still current?" check? Are double clicks blocked while a write is pending?
- **State:** can two copies of the same fact disagree? Is state written inside a getter, a derived value, or a render?
- **Time:** calendar days computed by date, not by subtracting 24 hours (DST)? Time zones explicit? Date strings parsed as local vs UTC on purpose?
- **Persistence:** does the change read data written by the previous version? What happens to old keys and old shapes? Is there a migration, and does it fail safe?
- **Errors:** is every failure either handled with a useful outcome or surfaced? No swallowed errors, no catch-all that hides a bug. Do error messages and logs avoid leaking secrets or personal data?
- **Security and egress:** does any new data leave the process or the device? Is it built from an allowlist? Are lookups by data-supplied keys safe from prototype keys (`__proto__`)?
- **Contracts:** wire formats, IPC payloads, storage schemas and public interfaces. Is a breaking change called out, and are both sides updated?

## 2. Data-driven design

The goal: adding a new case should mean adding **one entry**, not editing five files.

**Variation as branching vs as data**

```ts
// Smell: every new notification kind means editing this switch, and the
// copy, the route and the icon for one kind live in three different places.
function routeFor(kind: string) {
  if (kind === "levelUp") return "skills";
  if (kind === "weeklyWrap") return "weekly-wrap";
  if (kind === "reconnect") return "connectors";
  return "today";
}

// Better: one table owns everything about a kind; code reads it.
const NOTIFICATION_KINDS = {
  levelUp: { route: "skills", copy: levelUpCopy },
  weeklyWrap: { route: "weekly-wrap", copy: wrapCopy },
  reconnect: { route: "connectors", copy: reconnectCopy },
} as const satisfies Record<string, NotificationKind>;
```

**Invalid states made unrepresentable**

```ts
// Smell: four fields that can disagree. What does { isLoading: true, error: "x" } mean?
interface Patterns { isLoading: boolean; data?: Summary; error?: string; isCold?: boolean }

// Better: a discriminated union. Each state carries exactly what it needs.
type Patterns =
  | { state: "loading" }
  | { state: "failed" }
  | { state: "cold" }
  | { state: "ready"; summary: Summary };
```

**One source of truth**

- A value derived from other state (a count, a label, "is ready") should be computed where it's used, or memoized from its source. Storing it separately invites drift.
- A list duplicated in two places (route names in the router and in a preview list, kinds in a type and in a validator) should come from one definition, or one should be derived from the other, with a test that fails if they diverge.
- Prefer a schema that both validates and produces the type (zod, for instance) over a hand-written type plus a separate hand-written validator.

**Booleans and flags**

- Two booleans that interact usually hide a three- or four-state enum.
- A boolean parameter at a call site (`render(card, true, false)`) hides meaning. Prefer an options object or separate functions.

## 3. Abstractions

Use the `codebase-design` vocabulary.

- **Depth:** does the new module hide real complexity behind a small interface? Or is its interface nearly as large as its implementation (shallow)?
- **Deletion test:** if you inlined this module into its callers, would complexity spread to N places (it earns its keep) or simply disappear (a pass-through)? Flag wrappers that only rename or forward.
- **Seams:** is there a seam with only one adapter and no real variation behind it? That's a hypothetical seam. Was a dependency injected only so a test could replace it? Test at the real boundary instead.
- **Leaks:** must callers know an internal detail (a storage key format, an ordering rule, a magic string) to use the module correctly? Push that knowledge inside.
- **Ownership:** does logic live with the module that owns the data and its rules? A view computing business rules, or a composition root containing feature logic, is in the wrong place.
- **Generality:** is a "framework" built for one caller? Prefer the specific, simple version until a second case actually exists.

## 4. Human readability

- **Names:** taken from the domain glossary, consistent across the change, and saying what something is or does (`unseenSkillPoints`, not `getData2`). Booleans read as questions (`isStale`, `hasCards`). A name that needs a comment to explain it is the wrong name.
- **Local reasoning:** can a reader understand a function from its body plus its parameters' types? Hidden reads of global state, long chains of indirection, and "magic" ordering all force them to open other files.
- **Control flow:** early returns over nested conditionals. One level of abstraction per function. No clever one-liners that need a whiteboard.
- **Comments:** explain why something non-obvious is done, the invariant being protected, or the trade-off chosen. Comments that narrate what the code does are noise. Outdated comments are worse than none.
- **Consistency:** matches the surrounding idiom, file layout and error-handling style. A new pattern needs a reason, stated in the PR.
- **Size:** split a file or function when it holds more than one concern, not to hit a line count. A long function with one clear job is fine; three tiny functions that only call each other are not.

## 5. Tests

- Each new behavior has one primary test at the strongest boundary that reaches it (a page through its real runtime, a module through its interface), asserting what a user or caller observes.
- A bug fix has a regression test that would fail without the fix, for the intended reason.
- No tests of private functions or call shapes that a behavior-preserving refactor would break.
- No mocks that implement the behavior being asserted, and no fixtures that hand the code the answer.
- A negative test fails for the right reason, not because a different guard rejected first.
- Missing tests are findings only when they leave a real, credible risk uncovered.

## 6. Change hygiene

- Does the PR do one thing, or does it bundle unrelated changes that should be split?
- Is code that the change made dead actually deleted, along with its tests and exports?
- Are docs, glossaries, diagrams and data-flow notes updated where they describe what changed?
- No breadcrumbs ("moved to X" comments), no commented-out code, no TODOs without an owner or ticket.
