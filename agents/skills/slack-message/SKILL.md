---
name: slack-message
description: Writes a message meant for Slack and puts it on the macOS clipboard so it pastes into Slack with bold, italics, links, lists, quotes and code intact. Use whenever the user asks for a Slack message, Slack post, Slack reply, DM, thread reply, channel update or announcement for Slack, or asks to "write this for Slack" or "make this pasteable in Slack". Don't use for email, PR descriptions, docs, or when the user wants a message sent through a Slack integration instead of pasted by hand.
---

# Slack Message

Write the message in Markdown, copy it to the clipboard with the helper
script, and show it in the reply. The script turns the Markdown into HTML and
puts both HTML and plain text on the clipboard. Slack's message box keeps
formatting from pasted HTML, while Markdown pasted as plain text mostly shows
up as literal asterisks.

## 1. Write the message

The words are Petar's, so load and follow the `petar-writing` skill for voice.
Slack is conversational: lead with the point, keep it short, and skip
greetings and sign-offs unless the user asks for them.

Use only the Markdown that Slack can show:

- `**bold**`, `*italic*`, `~~strike~~`, `` `code` ``, fenced code blocks.
- `[text](https://url)` links, `-` bullet lists, `1.` numbered lists, `>` quotes.
- No headings. Slack has none; use a bold line instead.
- No tables or images. Turn a table into a short list.
- Paragraphs are separated by a blank line. A single line break inside a
  paragraph becomes a space, so put a blank line between lines that must stay
  separate, or end the line with `\` for a hard break.
- Emoji shortcodes such as `:tada:` work as typed.
- `@name` and `#channel` stay plain text after pasting. Slack only links them
  when picked from its autocomplete, so tell the user which ones to re-type.

## 2. Copy it to the clipboard

Write the message to a file in the session scratchpad (this avoids shell
quoting problems), then run the helper from this skill's directory:

```bash
~/.agents/skills/slack-message/scripts/slack-copy /path/to/scratchpad/slack-message.md
```

It needs macOS and `pandoc`. If it fails, show the error, still give the
message in the reply, and say that the clipboard was not updated. Do not
install pandoc for the user; suggest `brew install pandoc`.

## 3. Reply

Show the message as Markdown in the reply so the user can read it, then one
line saying it is on the clipboard. Add a line about any `@mentions` or
`#channels` to re-type. Nothing else.

When the user asks for changes, edit the file, run the script again, and show
the new version.
