# Fetch GitHub File

A Claude Code skill that fetches source code from GitHub URLs. Supports file contents, line ranges, directory listings, and full repository clones.

## When to use

Automatically activated when a GitHub URL is shared. Works with `github.com/owner/repo/blob/...`, `raw.githubusercontent.com/...`, or when asked to read/fetch code from GitHub.

## What it covers

- **URL Patterns**: Supported GitHub URL formats
- **Fetching Files**: Reading file contents, including specific line ranges (`#L10-L25`)
- **Fetching Directories**: Listing directory contents
- **Cloning Repositories**: Full repo clones when needed
- **URL Parsing**: How URLs are resolved to raw content
- **Error Handling**: Graceful handling of private repos, missing files, etc.

## Usage

```
/fetch-github-file https://github.com/owner/repo/blob/main/src/file.ex
```

Or just paste a GitHub URL in conversation.
