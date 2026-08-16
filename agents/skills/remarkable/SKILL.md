---
name: remarkable
description: Render a Markdown or Org document to a PDF for reading on Petar's reMarkable Paper Pure, in the clean classic CS/Lisp-paper aesthetic (Computer Modern Book, small-caps numbered sections, monochrome code, links as footnotes), using pandoc and tectonic. Use when the user asks for a PDF for the reMarkable, a "paper PDF", "Lisp paper aesthetic", a clean PDF of a Markdown or Org document, or to regenerate one after editing the source. Don't use for slides, HTML output, or when the user wants a specific other template.
---

Turn a Markdown or Org document into a PDF that looks like a classic CS
paper and reads well on e-ink: the MIT AI memo look, sized for the
reMarkable. New Computer Modern Book, 10pt article, small-caps section
headings, monochrome code, URLs as footnotes. Pandoc reads both formats;
the examples below say `DOC.md`, and an Org source only changes the two
spots called out in steps 4 and 5.

The source document is never modified. All transformation happens in a print
copy under the session scratchpad.

## 1. Check the toolchain

```bash
which pandoc tectonic
```

Both required. If either is missing, ask the user to install it
(`brew install pandoc tectonic`); do not install it for them.

## 2. Survey non-ASCII characters

Latin Modern Mono lacks box-drawing and arrow glyphs; they render as blanks.
Find what the document uses:

```bash
LC_ALL=C grep -o '[^ -~]' DOC.md | LC_ALL=C sort | LC_ALL=C uniq -c | LC_ALL=C sort -rn
```

`LC_ALL=C` on every stage is load-bearing: macOS locale collation silently
merges distinct Unicode punctuation, and the survey lies without it.

Curly quotes, en/em dashes, and ellipses are fine (Latin Modern has them).
Arrows and box-drawing characters must be transliterated in the print copy.

## 3. Make the print copy

Write to the scratchpad, never next to the source:

```bash
sed -E -e 's/──>/-->/g' -e 's/└─>/\\->/g' -e 's/→/->/g' \
       -e 's/─/-/g' -e 's/└/\\/g' -e 's/│/|/g' -e 's/├/+/g' \
       DOC.md > "$SCRATCH/DOC-print.md"
```

Extend the substitution list for whatever the survey found. Order matters:
replace multi-character sequences (`──>`, `└─>`) before their parts.

## 4. Decide the numbering

Check whether headings already carry their own numbers (`## 3. The publish
API`). Double numbering looks broken ("14.1  1. Publish one normal post").

- Headings are unnumbered prose: pass `--number-sections`.
- Headings self-number at every level: drop `--number-sections`.
- Mixed (numbered subsections under prose sections): keep `--number-sections`
  and strip the manual numbers in the print copy, e.g. add
  `-e 's/^### [0-9]+\. /### /'` to the sed above (for Org sources the
  heading marker is stars: `-e 's/^\*\* [0-9]+\. /** /'`). Only do this when
  the generated numbers still line up with any cross-references to the old
  ones.

## 5. Title, author, date

- Markdown opening with a single `#` title: pass
  `--shift-heading-level-by=-1`, so the H1 becomes the paper title and `##`
  become sections. Org with `#+title:`: the title is already metadata and
  `*` headings are already sections, so drop the shift flag entirely.
  Anything else: pass `-M title="..."`.
- No author. Petar is the only reader; a byline adds nothing.
- Date: today, spelled out ("August 6, 2026").

## 6. Render

The header is `assets/remarkable-header.tex`, next to this file. Use it via
the skill's base directory; do not inline a new one.

The page is sized to the screen of Petar's reMarkable Paper Pure (10.3in,
1872x1404, 226 PPI), so text renders at true point size with no zooming:
6.21in x 8.28in, 0.6in side margins (room for pen annotations), 0.55in top,
0.7in bottom with `footskip=0.3in`. The explicit footskip is what keeps the
page number off the bezel: LaTeX's default (~0.42in) is as deep as a thin
bottom margin, which parks the folio on the screen edge. The header sets
New Computer Modern in its Book weight: heavier strokes made for
low-contrast e-ink, where Latin Modern's hairlines go faint. Verbatim is
`\small` (~9pt against the 10pt body) so code stays prominent on e-ink;
that fits ~78-character lines in the column. The same PDF reads fine on a
laptop and prints acceptably; no separate profile.

```bash
pandoc "$SCRATCH/DOC-print.md" -o DOC.pdf \
  --pdf-engine=tectonic \
  --shift-heading-level-by=-1 \
  --number-sections \
  --syntax-highlighting=monochrome \
  -H "$SKILL_DIR/assets/remarkable-header.tex" \
  -V documentclass=article \
  -V fontsize=10pt \
  -V "geometry:paperwidth=6.21in,paperheight=8.28in,hmargin=0.6in,tmargin=0.55in,bmargin=0.7in,footskip=0.3in" \
  -V links-as-notes=true \
  -M date="..."
```

Notes:

- The command above is for Markdown; for an Org source drop
  `--shift-heading-level-by=-1` (step 5) and adjust any number-stripping
  sed to star headings (step 4). Everything else is identical.
- `--syntax-highlighting` needs pandoc >= 3.10; older pandoc calls it
  `--highlight-style`.
- The header loads `fancyvrb` explicitly. Without it, documents that have no
  fenced code blocks fail on `\fvset` with "Undefined control sequence".
- Tectonic downloads packages on first run; allow a generous timeout.
- The remarkable header loads New Computer Modern by OTF filename through
  fontspec. Tectonic's bundled `newcomputermodern.sty` is too old for the
  `[book]` package option; `\setmainfont{NewCM10-Book.otf}` works because
  tectonic resolves font files from its bundle by name.
- For handbook-style documents where each section is a chapter meant to be
  read alone (checklists with lessons, curricula), add
  `-H "$SKILL_DIR/assets/sectionbreak.tex"` as a second header: every
  section then starts on a fresh page. Leave it off for flowing documents
  like design plans.

## 7. Verify and hand off

- Overfull/underfull hbox warnings under ~2pt are cosmetic; ignore them.
  Larger overfulls usually mean a code line wider than ~78 characters; wrap
  it in the print copy.
- `ls -la` the PDF, then `open` it for the user.
- Be honest in the handoff: without poppler installed you verified the build,
  not the typography; the user is the visual check.
- Offer the exact regeneration command (or add it to a Makefile if the
  project has one and the user wants repeatability).
