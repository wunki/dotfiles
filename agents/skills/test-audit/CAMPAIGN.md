# Test-pruning campaign

Campaign mode prunes one subsystem's whole test surface in one change: a package,
service, plugin, or core feature area. The value bar, retention bar, candidate
evidence, and validation in [SKILL.md](SKILL.md) apply to every lane. This file adds
the order of work. Each step ends on its completion criterion; don't start the next
step early.

## 1. Baseline

At a pinned commit on the main branch, record the subsystem's test and test-support
line counts and every test file's pass or fail state. Keep baseline failures on their
own list. They are often real product bugs, not stale tests.

Done when every in-scope test file has a recorded baseline result.

## 2. Lanes and inventory

Split the surface into **lanes** along production owner boundaries, not file name
prefixes. Typical lanes: configuration, inbound handling, outbound handling,
persistence, transport, shared helpers, test harness, and end-to-end or QA scenarios.
Include the subsystem's cases in shared core suites and its e2e and harness tests.

Done when every test file and scenario the subsystem owns belongs to exactly one lane.

## 3. Read-only ledger per lane

Give each lane to its own read-only subagent when available; otherwise work the lanes
one at a time. Read every assigned test in full, including parameter tables. Read the
production owners and their entry points, callers, history, and CI routing. Put each
test declaration into a written **ledger** with one mark. A parametrized or
table-driven test is one declaration unless its rows need different marks; then mark
each row.

- `R`: retain, naming the contract and the bug it catches. A test that only moves to a
  better-named file stays `R` with the move noted.
- `F`: retain the contract but fix the assertion, such as a negative check that passes
  when only one of several items is missing.
- `C`: consolidate, naming the owner that absorbs the assertion: a sibling table case,
  a stronger boundary suite, or a shared owner elsewhere.
- `D`: delete, naming the proof that remains, or why no contract exists.

Judge a test by its assertions, not its name. A test named for clearing state can
assert that the state was not cleared.

Done when every declaration in the lane has a mark and an evidence line.

## 4. Layer plan per lane

The ledger is input, not the edit list. A second read-only pass looks for the
redundant **layer**: whole suites that replay a contract a stronger suite already
owns, such as several unit suites driving the same shared helper through one mock,
next to an integration suite that exercises it for real. Name the **keeper** suite for
each contract. Prefer a real boundary with a fake network or in-memory store over a
mocked collaborator. Correct ledger errors this pass finds.

Done when each lane plan names its retired files, its keeper per contract, the
assertions to carry into keepers, and the test-only production seams it unlocks.

## 5. Cutover

Edit lane by lane. Route changes to shared harnesses and support files through one
owner so lanes don't collide. With each lane, remove the test-only production seams it
unlocks: injection parameters, getters, reset exports, and indirection layers. Update
CI routing, sharding, test inventories, and snapshot directories that reference moved
or deleted files. Put durable test-ownership rules in the subsystem's `AGENTS.md` (or
equivalent), drawn only from mistakes this campaign actually found.

Done when every lane plan is applied and each lane's keepers pass.

## 6. Preservation review

Before claiming completion, have independent reviewers (fresh subagents, or a separate
pass with no memory of the edits) compare deleted coverage against the keepers, one
reviewer per boundary group. They look for contracts that lost their only proof, and
for new assertions that cannot fail, such as a rejection case production never reaches.

For each restored contract, make one deliberate **mutation** in the production owner
and confirm the keeper goes red. Then restore the source exactly.

Done when every reported gap is restored or rejected with source evidence, and every
restored contract has a caught mutation.

## 7. Product defects

A baseline failure that survives into a keeper is a bug report. Fix it at its owner in
a separate commit, and prove it through the real user flow with a **control** run that
reverts the fix and shows the old behavior. Record unrelated product issues as
follow-ups instead of fixing them in the campaign.

Done when each fixed defect has a failing control and a passing candidate on the same
harness.

## 8. Reconcile and hand off

Campaigns outlive many main-branch commits. Merge main rather than rebasing a long
campaign. When main changed a file the campaign deleted, keep the deletion, port the
new contract into the keeper, and confirm every regression test main added still has
a home. Rerun the whole subsystem suite on the merged head.

Review tooling may truncate the file list on a diff this large, so put the lane
summary in the PR description.

Hand off with the [SKILL.md](SKILL.md) report, plus:

- baseline and final test and support line counts, with production counted separately;
- lanes, retired layers, and keepers;
- preservation gaps found and the mutations that proved them;
- product defects with control and candidate proof.
