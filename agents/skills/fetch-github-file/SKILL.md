---
name: fetch-github-file
description: Fetches and displays source code from GitHub file and directory URLs using the gh CLI. Use when the user shares a github.com/owner/repo/blob/... or github.com/owner/repo/tree/... URL, a raw.githubusercontent.com URL, a GitHub Enterprise URL (e.g. github.mycompany.com/...), or explicitly asks to read, fetch, or show the contents of a file or directory on GitHub. Supports file contents, single-line anchors (#L10), line ranges (#L10-L25), directory listings, and shallow repository clones. Don't use for GitHub pull request URLs (/pull/), issue URLs (/issues/), commit URLs (/commit/), release URLs (/releases/), GitHub Actions workflow URLs (/actions/), Gist URLs (gist.github.com), or GitHub wiki URLs (/wiki/). Don't use when the user only wants to discuss, mention, or link to a GitHub project without requesting its file contents.
---

# Fetch GitHub File

Fetch source code from GitHub file and directory URLs using the `gh` CLI for authenticated access.

## Prerequisites

Verify that `gh` is available and authenticated before proceeding:

```bash
gh auth status
```

If `gh` is not installed or not authenticated, tell the user: "This skill requires the GitHub CLI (`gh`). Install it from https://cli.github.com and run `gh auth login`."

For GitHub Enterprise (GHE) hosts (e.g. `github.mycompany.com`), pass `--hostname github.mycompany.com` to all `gh` commands.

## URL Patterns

Parse the GitHub URL to determine what to fetch:

| URL Pattern | Type | Action |
|-------------|------|--------|
| `github.com/owner/repo/blob/ref/path` | File | Fetch file contents |
| `github.com/owner/repo/blob/ref/path#L10` | File + single line | Fetch file, show line 10 with context |
| `github.com/owner/repo/blob/ref/path#L10-L25` | File + range | Fetch file, show lines 10-25 |
| `github.com/owner/repo/tree/ref/path` | Directory | List directory contents |
| `github.com/owner/repo/tree/ref` | Repository root | List root directory |
| `github.com/owner/repo` | Repository | List root directory; offer clone for deeper exploration |
| `raw.githubusercontent.com/owner/repo/ref/path` | Raw file | Fetch via curl or gh api |

## URL Parsing

Extract components from a GitHub URL before fetching:

```
https://github.com/anthropics/claude-code/blob/main/src/index.ts#L10-L25
                   ^^^^^^^^^^^ ^^^^^^^^^^^^ ^^^^ ^^^^ ^^^^^^^^^^^^^ ^^^^^^^^
                   owner       repo         type ref  path          line range

type: "blob" = file, "tree" = directory
ref:  branch name, tag, or full commit SHA
line range: optional; #L10 = single line, #L10-L25 = range
```

For `raw.githubusercontent.com/owner/repo/ref/path`, extract owner, repo, ref, and path directly from the path segments.

For bare `github.com/owner/repo` URLs with no ref, omit the `?ref=` parameter entirely from API calls to use the repository's default branch.

## Fetching Files

```bash
# Fetch file content (returns JSON with base64-encoded content)
# Always quote the URL to prevent shell interpretation of ?
gh api 'repos/{owner}/{repo}/contents/{path}?ref={ref}'
```

Decode the response using the `@base64d` jq filter, which is portable across all platforms:

```bash
gh api 'repos/{owner}/{repo}/contents/{path}?ref={ref}' --jq '.content | @base64d'
```

### Raw URLs

For `raw.githubusercontent.com` URLs, use curl (no auth required for public repos) or the API path above:

```bash
curl -fsSL 'https://raw.githubusercontent.com/{owner}/{repo}/{ref}/{path}'
```

### Line Anchors and Ranges

If the URL includes `#L10` (single line) or `#L10-L25` (range):

1. Fetch the full file.
2. Extract and display only the specified lines.
3. Prefix each line with its number.

For a single-line anchor (`#L10`), display lines 10 ± 5 for context and highlight line 10.

Example output format:

```
# src/utils/parser.ts (lines 10-25)

10: export function parseUrl(url: string): ParsedUrl {
11:   const match = url.match(GITHUB_PATTERN);
...
```

## Fetching Directories

```bash
gh api 'repos/{owner}/{repo}/contents/{path}?ref={ref}' \
  --jq '.[] | "\(.type)\t\(.name)"'
```

Display as a tree structure showing files and subdirectories. When offering to fetch a specific file from the listing, construct the full path as `{parent-path}/{name}` so the next API call can be issued without ambiguity.

## Cloning Repositories

Clone only when the user explicitly asks for full repo access, or when they need to explore multiple files across the repository. Do not clone when a single file or directory fetch is sufficient.

```bash
TEMP_DIR=$(mktemp -d)
gh repo clone {owner}/{repo} "$TEMP_DIR" -- --depth 1 --branch {ref}
echo "Cloned to: $TEMP_DIR"
```

Use `--depth 1` (shallow clone) by default. After cloning, list the root structure and read files as requested. Keep the temp directory path for the remainder of the session.

If `--branch {ref}` fails because ref is a commit SHA, fetch that specific commit then checkout:

```bash
gh repo clone {owner}/{repo} "$TEMP_DIR" -- --depth 1
git -C "$TEMP_DIR" fetch --depth 1 origin {ref}
git -C "$TEMP_DIR" checkout {ref}
```

Note: `git checkout {ref}` after a plain `--depth 1` clone will fail for arbitrary commit SHAs that are not the shallow tip. The `fetch --depth 1 origin {ref}` step is required.

## Error Handling

- **404**: File or path does not exist at the given ref. Verify the path spelling and that the ref (branch/tag/commit) exists.
- **403 / rate limited**: Run `gh auth status` to confirm authentication. If authenticated and still 403, the repo may be private and the user's token may lack the required scope.
- **Large file (content is null, download_url is present)**: The file exceeds the GitHub API 1 MB limit. Fetch via the `download_url` from the response (pre-signed for private repos, no extra auth needed):
  ```bash
  curl -fsSL '{download_url}'
  ```
- **gh not installed**: Inform the user to install from https://cli.github.com.
- **gh not authenticated**: Run `gh auth login` or set the `GITHUB_TOKEN` environment variable.
- **GitHub Enterprise**: Pass `--hostname {ghe-hostname}` to all `gh` commands.

## Workflow

1. Verify `gh auth status` — stop and inform the user if `gh` is missing or unauthenticated.
2. Parse the URL — extract owner, repo, type (blob/tree/raw), ref, path, and line anchor.
3. Determine fetch strategy based on URL type (file, raw file, directory, or repository root).
4. Fetch via `gh api` or `curl` for raw URLs.
5. Format output — file contents with line numbers, or directory listing as a tree.
6. Offer next steps — suggest fetching another file, exploring a directory, or cloning the repo.

## Examples

**User pastes**: `https://github.com/anthropics/claude-code/blob/main/src/index.ts#L1-L50`

1. Parse: owner=anthropics, repo=claude-code, ref=main, path=src/index.ts, lines=1-50
2. Run: `gh api 'repos/anthropics/claude-code/contents/src/index.ts?ref=main' --jq '.content | @base64d'`
3. Display lines 1-50 with line numbers.

**User pastes**: `https://github.com/anthropics/claude-code/tree/main/src`

1. Parse: owner=anthropics, repo=claude-code, ref=main, path=src
2. Run: `gh api 'repos/anthropics/claude-code/contents/src?ref=main' --jq '.[] | "\(.type)\t\(.name)"'`
3. Display directory listing; when offering to fetch a file, use full path `src/{name}`.

**User pastes**: `https://raw.githubusercontent.com/anthropics/claude-code/main/package.json`

1. Parse: owner=anthropics, repo=claude-code, ref=main, path=package.json
2. Run: `curl -fsSL 'https://raw.githubusercontent.com/anthropics/claude-code/main/package.json'`
3. Display file contents.

**User pastes**: `https://github.com/anthropics/claude-code`

1. Parse: owner=anthropics, repo=claude-code, no ref, no path
2. Run: `gh api 'repos/anthropics/claude-code/contents/'` (no ?ref= parameter)
3. Display root directory listing and offer to fetch specific files or clone for deeper exploration.
