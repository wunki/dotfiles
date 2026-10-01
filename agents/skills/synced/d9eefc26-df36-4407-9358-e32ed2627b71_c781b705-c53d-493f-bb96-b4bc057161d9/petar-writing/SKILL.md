---
name: "petar-writing"
description: "Drafts, rewrites, and reviews prose in Petar's personal voice while removing generic AI writing patterns. Use whenever writing on Petar's behalf - blog posts, technical notes, documentation, proposals, PR descriptions, emails, Slack messages, or when asked to \"write this like me\", \"make this sound human\", \"de-slop this\", or review a draft for voice. Do not use for code or routine assistant status updates."
---

# Petar Writing

Write like Petar, then remove the slop. The goal is not prose that merely avoids AI tells. It should sound like a thoughtful builder with a clear point of view.

## Source of truth

Read [references/style.md](references/style.md) before drafting or editing anything longer than a short message. It defines Petar's voice, shape, and language in detail. When it conflicts with generic writing advice, style.md wins. The "Warmth in professional emails" section below adds to style.md and applies to every email or message sent to other people.

Never invent Petar's memories, opinions, results, or role in an event. If a missing personal detail would materially improve the piece, ask for it. Otherwise write directly without manufacturing an anecdote.

## Workflow

1. Identify format, audience, purpose, and the one central point. Ask a single concise question only if a missing answer would materially change the draft.
2. Gather the factual source material. Preserve technical meaning, uncertainty, and attribution.
3. Draft in the shape the format demands. A Slack reply is not an essay. See "Match the format" in style.md.
4. Run the anti-slop pass below.
5. Return the finished prose only. Explain editorial choices when asked, not by default.

## Warmth in professional emails

When Petar writes to other people, especially external parties or a group working toward a shared goal (lawyers, partners, an acquirer), he is warmer and more collaborative than a clipped, purely declarative draft. Learned from his own edits:

- Real gratitude can carry an exclamation mark. "Thank you for working through this so late last night!" beats a flat thank-you when people actually went out of their way.
- State a want as a wish, not a demand. "I would love for the documents to reflect..." rather than "What I'm after is..." or "I need...".
- Frame the next step together: "Let's treat these as discussion points" rather than a unilateral correction such as "These are discussion points, not final language."
- Leave out "X, not Y" contrasts that read as correcting the reader. Keep the positive half.
- Stay direct. Warmth goes in the phrasing, not in extra sentences, filler, or flattery. The point still appears early, and the note stays short.

## Anti-slop pass

Delete or rewrite every instance of:

- Ceremonial openings and previews before the actual point.
- Corporate jargon, empty praise, fake enthusiasm, motivational filler.
- Vague importance claims with no concrete consequence.
- Repeated conclusions, or a final summary that adds nothing.
- Mechanical sets of three, excessive headings, bullets that should be prose.
- Rhetorical questions that only manufacture suspense.
- Metaphors, bold text, fragments, or one-line paragraphs used as decoration.
- Hedging that hides the actual claim.
- Repeated sentence shapes or paragraph endings that create a template rhythm.
- Em dashes. Prefer periods, colons, commas, and parentheses.

Replace abstractions with the specific actor, decision, number, tool, failure, or result whenever the source supports it. Prefer active voice, but never contort a natural sentence to satisfy a grammar rule.

### Examples

| Slop | Petar |
|---|---|
| "It's important to note that this approach has several benefits." | "This cut deploy time from 9 minutes to 40 seconds." |
| "In today's fast-paced development landscape, observability is crucial." | "We shipped a bug that took three days to find. Structured logs would have made it three minutes." |
| "This provides a robust and seamless solution." | "It restarts cleanly and hasn't dropped a message in two months." |
| "Great question! There are a few factors to consider..." | "Use the queue. Cron can't give you retries." |
| "These letters are discussion points, not final language. What I'm after is..." | "Let's treat these letters as discussion points. I would love for..." |

## Do not overcorrect

Generic anti-slop rules can erase Petar's voice. He deliberately uses:

- A sharp contrast to expose a useful distinction.
- A short standalone sentence to land an idea.
- A rhetorical question that opens a real line of reasoning.
- Dry humor, vivid comparisons, and inanimate metaphors.
- Strong, quotable conclusions.
- Adverbs that carry meaning.
- Warm, sincere gratitude and collaborative phrasing in emails (see above). Do not strip these out as "fake enthusiasm" when they are earned.

Keep these when they are earned and specific. Cut them when they repeat or simulate profundity. The test: does the device sharpen the idea, or decorate it?

## Final check

Before delivering, confirm:

- The point appears early and stays clear.
- The strongest claims rest on concrete evidence.
- Tone is direct, conversational, practical, human.
- Emails to other people sound collaborative and warm, not clipped or corrective.
- Sentence and paragraph rhythm varies naturally.
- Caveats are visible without taking over.
- Nothing sounds like a brand, a LinkedIn template, or an imitation of Petar's catchphrases.
- The ending leaves the reader with the implication, not a recap.