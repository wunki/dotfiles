---
name: test-audit
description: >
  Prunes low-value tests in any codebase: tests that re-assert source, duplicate stronger
  proof, couple to implementation, or keep test-only production seams alive. Also gates
  new tests against the same value bar. Use when asked to audit, prune, trim, dedupe, or
  clean up tests, reduce test count or suite runtime without losing confidence, review
  whether tests are worth keeping, or check if a new test earns its place. Don't use for
  writing tests for new features, fixing a failing test, raising coverage, or setting up
  test infrastructure.
---

# Test Audit

One value bar, three modes:

- **Audit** (default): a focused sweep for a few high-confidence candidates in one
  owner area. Land each coherent batch on its own; optimize for confidence, not
  deletion count.
- **Campaign**: prune one whole subsystem's test surface (every test file a
  package, service, or feature area owns). Read [CAMPAIGN.md](CAMPAIGN.md) first.
- **Authoring gate**: judge a new or changed test before it lands.

## Orient first

Before judging anything, learn how this repo tests:

- Read root and scoped `AGENTS.md`, `CLAUDE.md`, and contributing docs for test rules.
- Find the real test commands in CI config (`.github/workflows`, `.gitlab-ci.yml`, etc.),
  then `package.json` scripts, `Makefile`, `justfile`, `mix.exs`, `pyproject.toml`,
  `Cargo.toml`, or equivalent. CI is the source of truth.
- Note coverage thresholds, test inventories, snapshot directories, and CI sharding
  that reference test files by path. Deletions must keep these consistent.
- Record the baseline: run the in-scope tests at a pinned commit and note which fail.

## Value bar

A test justifies its maintenance cost by protecting observable behavior, a credible
regression, or an independently meaningful contract. A test that must change for a
behavior-preserving refactor is suspect, not automatically deletable.

Before judging a candidate, read the complete test and its production owner: entry
point, callers, callees, sibling implementations, overlapping tests, and relevant
history (`git log -L`, `git blame`, the PR or issue that added it). When a test claims
dependency-backed behavior, check the dependency source or types directly.

## Junk patterns

Audits hunt for these; the authoring gate rejects a new test that matches one.

- assertion-free coverage probes (runs code, asserts nothing meaningful);
- self-comparisons and identity copiers;
- copied fixtures, inventories, manifests, or export lists;
- exact source, import, or string greps;
- snapshots of trivial or unstable output that nobody reviews;
- private-function or call-shape tests duplicated at a real boundary;
- duplicate invocations of the same contract, often copy-pasted with one value changed;
- per-caller replays of a shared helper's tests;
- tests whose only purpose is preserving test-only exports, globals, or wrappers;
- dead production code whose only callers are tests;
- expected values computed by the function under test;
- mocks that implement the asserted behavior, or one mock standing in for different APIs;
- fixtures that supply the result, ordering, or callback the code under test should
  produce, or persistence asserted against a store the path never writes;
- tests that restate a declared config or flag instead of exercising what it promises;
- negative tests that pass for an unrelated reason, such as a different guard
  rejecting first or an error path production never reaches;
- names that promise more than the assertions check, such as a "clears the cache"
  test that asserts the cache was not cleared.

Search hints (adapt to the language): tests with no `assert`/`expect`; tests that
import from internal or private paths; exports named like `_forTesting`, `__test__`,
`resetForTests`, or annotated `@VisibleForTesting`; mocks of the module under test;
near-identical test bodies; snapshot files with no recent intentional updates.

## Retention bar

Keep a test when it independently enforces a public API, protocol, wire or file
format, config, migration, storage, security, platform, default value, or architecture
contract. Also keep:

- call ordering when the order is observable behavior;
- regression tests with a credible failure mode;
- source inspection when it is the cheapest independent guard: it fails when the
  contract changes (the user-facing key, byte, or path) and survives a rename;
- a test that fails on the baseline: treat it as a possible product bug, reproduce
  it, and fix the owner instead of deleting it.

Static or slow is not a deletion reason. A test that looks like implementation may
still be the only independent proof of a contract; prove otherwise before removing it.

## Discovery

Keep discovery read-only and report evidence before editing. For broad scope, split
the work into lanes along production owner boundaries (packages, services, modules,
UI, scripts and tooling) plus one cross-cutting pattern sweep. Run lanes as parallel
read-only subagents when available.

Outside campaign mode, prefer a few high-confidence candidates over a large
speculative inventory.

## Candidate evidence

Record every field before editing. A missing field means the candidate is not ready:

- exact test name and location;
- the failure it can actually detect;
- non-test callers of the production or support code it covers;
- the stronger remaining proof at the owner boundary, or why no proof is needed;
- history and the reason the test or seam exists;
- production or test-support code its deletion unlocks;
- risk and the focused validation command.

## Edit shape

Pick one coherent owner-boundary batch. Delete obsolete test-only exports, globals,
wrappers, and dead production paths instead of keeping aliases. Move retained
regressions to their canonical owner. Fold near-duplicates into one table-driven or
parametrized test. Consolidate repeated setup into shared fixtures.

Prefer net-negative production lines. Don't add replacement tests that restate the same
implementation, and don't turn uncertain candidates into deletions to raise the count.

## Authoring gate

Before adding or keeping a new test, answer four questions. A missing answer means
don't add it yet:

1. What observable behavior, invariant, or independent contract does it protect?
2. What credible regression makes it fail?
3. Why doesn't existing coverage already catch that? Each contract has one primary
   test at the strongest boundary; another layer needs its own risk, such as a
   transport or lifecycle failure the primary test can't reach. Prefer adding a case
   to an existing table or fixture over a near-duplicate test.
4. Does it need a production seam (export, flag, wrapper, injection hook) that no
   production caller needs? If yes, test at the real boundary instead.

Then check it against every [junk pattern](#junk-patterns). A test that would break
under a behavior-preserving refactor asserts implementation; rewrite it at the owning
boundary.

A bug regression test must fail on the pre-fix code for the intended reason and pass
after the fix. One regression at the owner boundary covers the bug; don't replay the
scenario at every layer it crosses.

## Validation

Don't edit source or tests while a watch-mode test runner is active in the checkout.

1. Run the smallest owner and sibling tests with the repo's own runner, scoped to the
   touched paths.
2. For any deletion that leans on a keeper, make one deliberate mutation in the
   production owner, confirm the keeper goes red, then restore the source exactly.
3. When a deleted test checked a script, build step, or generated output, run that
   real script or a dry run of it.
4. Run the repo's formatter and linter on changed files, then `git diff --check`.
5. Run whatever changed-files or full gate CI requires. If a coverage threshold drops,
   show the lost lines are dead or covered elsewhere. Never lower a threshold without
   asking.
6. Inspect `git diff --numstat` and report production lines separately from test and
   test-support lines.

## Landing

Commit, push, or open a PR only when the user asks. Land one coherent batch at a time.
After it lands, refresh from the main branch and rerun read-only discovery for the next
batch.

## Handoff

Report:

- removed low-value categories and why they existed;
- production simplifications unlocked;
- false positives kept and the contract each still guards;
- validation actually run, including mutation checks;
- production versus test lines changed;
- commit or PR state, if any;
- named follow-ups and any baseline failures that look like product bugs.
