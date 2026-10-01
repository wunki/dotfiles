---
name: workera-slides
title: Workera Slides
description: Create, edit, and iterate on polished Workera-branded PowerPoint (.pptx) decks. Use this skill any time the user asks to build, update, or redesign a slide deck, presentation, or .pptx file in a Workera context — including pitch decks, training materials, enablement decks, and internal presentations. Also trigger when the user mentions "slides", "deck", "PowerPoint", or "pptx" and is working within Workera's brand. Always use this skill before writing any slide-related code or creating any .pptx file — it defines the required workflow and brand rules.
author_email: zoie@workera.ai
author_name: Zoie Wolfe
tags: slides, pptx, presentations
---

# Workera Slides Skill

Produces polished, on-brand Workera PowerPoint (.pptx) decks efficiently.
Prioritizes visual clarity, Workera brand consistency, and concise messaging.

---

## Mandatory Pre-Flight (do this every time, no exceptions)

1. **Read the PPTX skill** at `/mnt/skills/public/pptx/SKILL.md` before writing any code.
2. **Read the Workera Template PDF** at `/mnt/project/TEMPLATE_Workera_Slides_Template_2025.pdf`
   — it contains 100+ slides. Identify the best-matching layouts for the current request.
   Do NOT use every slide; curate only the most relevant patterns.
3. Extract exact hex codes, font names, and layout logic from the template. Never approximate.

---

## Brand Rules

| Element | Guidance |
|---|---|
| Backgrounds | Deep plum for section dividers and title slides; cream/off-white for content slides |
| Accent colors | Magenta and orange — use for emphasis, icons, key data points |
| Typography | Bold sans-serif headers; clean body text; match weights exactly from template |
| Logo placement | Follow template positioning — typically top-left or bottom-right corner |
| Visual style | High contrast, dramatic, strong hierarchy — NOT generic corporate |

Match hex codes, font choices, and heading styles **exactly** as shown in the template.
Flag any color or font decision that can't be directly confirmed from the template.

---

## Design Principles

- **Minimal text per slide** — one strong idea per slide, supported by visuals or icons
- **Bold headers** — the header should communicate the point, not just label the slide
- **Visual hierarchy first** — size, color, and whitespace do the heavy lifting
- **Character welcome** — humor and personality are on-brand when context allows
- **Icons and visuals** — prefer over bullet lists wherever possible

---

## Workflow

### Step 1 — Scope the deck
- Parse the user's request to determine slide count, purpose, and audience.
- For decks **8 slides or more**: confirm the proposed outline with the user before building.
- For decks under 8 slides: proceed, but briefly state the structure you're using.

### Step 2 — Template analysis
- Open the Workera Template PDF.
- Identify candidate slides for: title/cover, section dividers, content layouts, summary/closing.
- Note which slide numbers/patterns you're drawing from (helps with iteration later).

### Step 3 — Build
- Follow all instructions in `/mnt/skills/public/pptx/SKILL.md`.
- Use pptxgenjs (Node.js) unless another method is specified.
- Output a `.pptx` file to `/mnt/user-data/outputs/`.
- Use `present_files` to deliver the file.

### Step 4 — Deliver with context
- Name the file clearly (e.g., `Workera_Q2_Enablement_Deck.pptx`).
- Note any open items, layout tradeoffs, or placeholder content flagged for follow-up.
- Invite specific feedback rather than asking "does this look good?"

---

## Editing Existing Decks

If the user provides an existing `.pptx` file:
1. Read it first using the pptx skill's extraction approach before making changes.
2. Preserve what's working — apply changes precisely, don't rebuild slides that don't need it.
3. Call out anything that's off-brand in the existing file rather than silently "fixing" it.

---

## Speaker Notes

When requested:
- Keep notes punchy and practical — talking points, not a script.
- One or two sentences per slide is usually right.
- Write them as if the presenter is confident, not reading.

---

## Iteration Protocol

Zoie iterates frequently. When applying feedback:
- Apply changes precisely to what was called out.
- Don't touch slides that weren't mentioned.
- Confirm your interpretation of ambiguous feedback before rebuilding.

---

## Flags & Escalations

Raise a flag (don't guess) when:
- A color, font, or layout can't be confirmed from the template
- The requested slide count or structure seems off for the content
- A slide is content-heavy enough that a layout decision could go multiple ways
- Placeholder content (like `(link TBD)`) is left in — note it explicitly in delivery

---

## Known Workera Deck Context

The following may be relevant for internal decks:

- **Deal Registration Workflow deck** — previously built (`Partner_Deal_Registration_Workflow.pptx`), 10 slides, covers a Salesforce-based partner workflow. Two open items from that build: Christine Lommatsch's name wrapping in the queue table (Slide 4), and a placeholder `(link TBD)` on Slide 5 for the Partner Deals report link.
- **Key stakeholders**: Tim DaRosa (visibility), Jim Hemgen & Christine Lommatsch (deal registration owners), Vignesh Ganesan (Salesforce Architect)
