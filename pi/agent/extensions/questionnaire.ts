/**
 * Interactive pickers for asking the user questions.
 *
 * Two tools share one renderer:
 * - `question`: one question, options given as plain labels.
 * - `questionnaire`: one or more questions with ids and values; several
 *   questions use tabs and finish on a review screen.
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import {
	Editor,
	type EditorTheme,
	Key,
	matchesKey,
	Text,
	visibleWidth,
	wrapTextWithAnsi,
} from "@earendil-works/pi-tui";
import { Type } from "typebox";

interface QuestionOption {
	value: string;
	label: string;
	description?: string;
}

type RenderOption = QuestionOption & { isOther?: boolean };

interface Question {
	id: string;
	label: string;
	prompt: string;
	options: QuestionOption[];
	allowOther: boolean;
}

interface Answer {
	id: string;
	value: string;
	label: string;
	wasCustom: boolean;
	index?: number;
}

interface QuestionnaireResult {
	questions: Question[];
	answers: Answer[];
	cancelled: boolean;
}

interface QuestionDetails {
	question: string;
	options: string[];
	answer: string | null;
	wasCustom?: boolean;
}

const OTHER_LABEL = "Type something.";

const QuestionOptionSchema = Type.Object({
	value: Type.String({ description: "The value returned when selected" }),
	label: Type.String({ description: "Display label for the option" }),
	description: Type.Optional(Type.String({ description: "Optional description shown below label" })),
});

const QuestionSchema = Type.Object({
	id: Type.String({ description: "Unique identifier for this question" }),
	label: Type.Optional(
		Type.String({
			description: "Short contextual label for tab bar, e.g. 'Scope', 'Priority' (defaults to Q1, Q2)",
		}),
	),
	prompt: Type.String({ description: "The full question text to display" }),
	options: Type.Array(QuestionOptionSchema, { description: "Available options to choose from" }),
	allowOther: Type.Optional(Type.Boolean({ description: "Allow 'Type something' option (default: true)" })),
});

const QuestionnaireParams = Type.Object({
	questions: Type.Array(QuestionSchema, { description: "Questions to ask the user" }),
});

const SimpleOptionSchema = Type.Object({
	label: Type.String({ description: "Display label for the option" }),
	description: Type.Optional(Type.String({ description: "Optional description shown below label" })),
});

const QuestionParams = Type.Object({
	question: Type.String({ description: "The question to ask the user" }),
	options: Type.Array(SimpleOptionSchema, { description: "Options for the user to choose from" }),
});

function textResult<T>(text: string, details: T): { content: { type: "text"; text: string }[]; details: T } {
	return { content: [{ type: "text", text }], details };
}

/**
 * Show the picker and resolve with the answers. Assumes `ctx.mode === "tui"`
 * and at least one question.
 */
function runPicker(ctx: ExtensionContext, questions: Question[]): Promise<QuestionnaireResult> {
	const isMulti = questions.length > 1;
	const totalTabs = questions.length + 1; // Include the final review tab.

	return ctx.ui.custom<QuestionnaireResult>((tui, theme, _kb, done) => {
		let currentTab = 0;
		let optionIndex = 0;
		let inputMode = false;
		let inputQuestionId: string | null = null;
		let cachedLines: string[] | undefined;
		const answers = new Map<string, Answer>();

		const editorTheme: EditorTheme = {
			borderColor: (s) => theme.fg("accent", s),
			selectList: {
				selectedPrefix: (t) => theme.fg("accent", t),
				selectedText: (t) => theme.fg("accent", t),
				description: (t) => theme.fg("muted", t),
				scrollInfo: (t) => theme.fg("dim", t),
				noMatch: (t) => theme.fg("warning", t),
			},
		};
		const editor = new Editor(tui, editorTheme);

		function refresh() {
			cachedLines = undefined;
			tui.requestRender();
		}

		function submit(cancelled: boolean) {
			done({ questions, answers: Array.from(answers.values()), cancelled });
		}

		function currentQuestion(): Question | undefined {
			return questions[currentTab];
		}

		function currentOptions(): RenderOption[] {
			const q = currentQuestion();
			if (!q) return [];
			const opts: RenderOption[] = [...q.options];
			if (q.allowOther) {
				opts.push({ value: "__other__", label: OTHER_LABEL, isOther: true });
			}
			return opts;
		}

		function allAnswered(): boolean {
			return questions.every((q) => answers.has(q.id));
		}

		function leaveInputMode() {
			inputMode = false;
			inputQuestionId = null;
			editor.setText("");
		}

		function advanceAfterAnswer() {
			if (!isMulti) {
				submit(false);
				return;
			}
			currentTab = currentTab < questions.length - 1 ? currentTab + 1 : questions.length;
			optionIndex = 0;
			refresh();
		}

		function saveAnswer(questionId: string, value: string, label: string, wasCustom: boolean, index?: number) {
			answers.set(questionId, { id: questionId, value, label, wasCustom, index });
		}

		editor.onSubmit = (value) => {
			if (!inputQuestionId) return;
			const trimmed = value.trim();
			if (!trimmed) {
				// An empty answer returns to the option list instead of recording nothing.
				leaveInputMode();
				refresh();
				return;
			}
			saveAnswer(inputQuestionId, trimmed, trimmed, true);
			leaveInputMode();
			advanceAfterAnswer();
		};

		function handleInput(data: string) {
			if (inputMode) {
				if (matchesKey(data, Key.escape)) {
					leaveInputMode();
					refresh();
					return;
				}
				editor.handleInput(data);
				refresh();
				return;
			}

			const q = currentQuestion();
			const opts = currentOptions();

			if (isMulti) {
				if (matchesKey(data, Key.tab) || matchesKey(data, Key.right)) {
					currentTab = (currentTab + 1) % totalTabs;
					optionIndex = 0;
					refresh();
					return;
				}
				if (matchesKey(data, Key.shift("tab")) || matchesKey(data, Key.left)) {
					currentTab = (currentTab - 1 + totalTabs) % totalTabs;
					optionIndex = 0;
					refresh();
					return;
				}
			}

			if (currentTab === questions.length) {
				if (matchesKey(data, Key.enter) && allAnswered()) {
					submit(false);
				} else if (matchesKey(data, Key.escape)) {
					submit(true);
				}
				return;
			}

			if (matchesKey(data, Key.up)) {
				optionIndex = Math.max(0, optionIndex - 1);
				refresh();
				return;
			}
			if (matchesKey(data, Key.down)) {
				optionIndex = Math.min(opts.length - 1, optionIndex + 1);
				refresh();
				return;
			}

			if (matchesKey(data, Key.enter) && q) {
				const opt = opts[optionIndex];
				if (opt.isOther) {
					inputMode = true;
					inputQuestionId = q.id;
					editor.setText("");
					refresh();
					return;
				}
				saveAnswer(q.id, opt.value, opt.label, false, optionIndex + 1);
				advanceAfterAnswer();
				return;
			}

			if (matchesKey(data, Key.escape)) {
				submit(true);
			}
		}

		function render(width: number): string[] {
			if (cachedLines) return cachedLines;

			const lines: string[] = [];
			const renderWidth = Math.max(1, width);
			const q = currentQuestion();
			const opts = currentOptions();

			function addWrapped(text: string) {
				lines.push(...wrapTextWithAnsi(text, renderWidth));
			}

			function addWrappedWithPrefix(prefix: string, text: string) {
				const prefixWidth = visibleWidth(prefix);
				if (prefixWidth >= renderWidth) {
					addWrapped(prefix + text);
					return;
				}
				const wrapped = wrapTextWithAnsi(text, renderWidth - prefixWidth);
				const continuationPrefix = " ".repeat(prefixWidth);
				for (let i = 0; i < wrapped.length; i++) {
					lines.push(`${i === 0 ? prefix : continuationPrefix}${wrapped[i]}`);
				}
			}

			lines.push(theme.fg("accent", "─".repeat(renderWidth)));

			if (isMulti) {
				const tabs: string[] = ["← "];
				for (let i = 0; i < questions.length; i++) {
					const isActive = i === currentTab;
					const isAnswered = answers.has(questions[i].id);
					const text = ` ${isAnswered ? "■" : "□"} ${questions[i].label} `;
					const styled = isActive
						? theme.bg("selectedBg", theme.fg("text", text))
						: theme.fg(isAnswered ? "success" : "muted", text);
					tabs.push(`${styled} `);
				}
				const isSubmitTab = currentTab === questions.length;
				const submitText = " ✓ Submit ";
				const submitStyled = isSubmitTab
					? theme.bg("selectedBg", theme.fg("text", submitText))
					: theme.fg(allAnswered() ? "success" : "dim", submitText);
				tabs.push(`${submitStyled} →`);
				addWrappedWithPrefix(" ", tabs.join(""));
				lines.push("");
			}

			function renderOptions() {
				for (let i = 0; i < opts.length; i++) {
					const opt = opts[i];
					const selected = i === optionIndex;
					const isOther = opt.isOther === true;
					const prefix = selected ? theme.fg("accent", "> ") : "  ";
					const label = `${i + 1}. ${opt.label}${isOther && inputMode ? " ✎" : ""}`;
					const color = selected || (isOther && inputMode) ? "accent" : "text";

					addWrappedWithPrefix(prefix, theme.fg(color, label));
					if (opt.description) {
						addWrappedWithPrefix("     ", theme.fg("muted", opt.description));
					}
				}
			}

			if (inputMode && q) {
				addWrappedWithPrefix(" ", theme.fg("text", q.prompt));
				lines.push("");
				renderOptions();
				lines.push("");
				addWrappedWithPrefix(" ", theme.fg("muted", "Your answer:"));
				for (const line of editor.render(Math.max(1, renderWidth - 2))) {
					lines.push(` ${line}`);
				}
				lines.push("");
				addWrappedWithPrefix(" ", theme.fg("dim", "Enter to submit • Esc to go back"));
			} else if (currentTab === questions.length) {
				addWrappedWithPrefix(" ", theme.fg("accent", theme.bold("Ready to submit")));
				lines.push("");
				for (const question of questions) {
					const answer = answers.get(question.id);
					if (answer) {
						const prefix = answer.wasCustom ? "(wrote) " : "";
						const summary = `${theme.fg("muted", `${question.label}: `)}${theme.fg("text", prefix + answer.label)}`;
						addWrappedWithPrefix(" ", summary);
					}
				}
				lines.push("");
				if (allAnswered()) {
					addWrappedWithPrefix(" ", theme.fg("success", "Press Enter to submit"));
				} else {
					const missing = questions
						.filter((q) => !answers.has(q.id))
						.map((q) => q.label)
						.join(", ");
					addWrappedWithPrefix(" ", theme.fg("warning", `Unanswered: ${missing}`));
				}
			} else if (q) {
				addWrappedWithPrefix(" ", theme.fg("text", q.prompt));
				lines.push("");
				renderOptions();
			}

			lines.push("");
			if (!inputMode) {
				const help = isMulti
					? "Tab/←→ navigate • ↑↓ select • Enter confirm • Esc cancel"
					: "↑↓ navigate • Enter select • Esc cancel";
				addWrappedWithPrefix(" ", theme.fg("dim", help));
			}
			lines.push(theme.fg("accent", "─".repeat(renderWidth)));

			cachedLines = lines;
			return lines;
		}

		return {
			render,
			invalidate: () => {
				cachedLines = undefined;
			},
			handleInput,
		};
	});
}

export default function questionTools(pi: ExtensionAPI) {
	pi.registerTool({
		name: "questionnaire",
		label: "Questionnaire",
		description:
			"Ask one or more questions to clarify requirements, record preferences, or confirm a decision. Multiple questions use a tabbed picker.",
		parameters: QuestionnaireParams,

		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const cancelled = (questions: Question[] = []): QuestionnaireResult => ({
				questions,
				answers: [],
				cancelled: true,
			});

			if (ctx.mode !== "tui") {
				return textResult("Error: UI not available (running in non-interactive mode)", cancelled());
			}
			if (params.questions.length === 0) {
				return textResult("Error: No questions provided", cancelled());
			}

			const questions: Question[] = params.questions.map((q, i) => ({
				...q,
				label: q.label || `Q${i + 1}`,
				allowOther: q.allowOther !== false,
			}));

			const result = await runPicker(ctx, questions);

			if (result.cancelled) {
				return textResult("User cancelled the questionnaire", result);
			}

			const answerLines = result.answers.map((a) => {
				const qLabel = questions.find((q) => q.id === a.id)?.label || a.id;
				return a.wasCustom
					? `${qLabel}: user wrote: ${a.label}`
					: `${qLabel}: user selected: ${a.index}. ${a.label}`;
			});

			return textResult(answerLines.join("\n"), result);
		},

		renderCall(args, theme, _context) {
			const qs = (args.questions as Question[]) || [];
			const count = qs.length;
			const labels = qs.map((q) => q.label || q.id).join(", ");
			let text = theme.fg("toolTitle", theme.bold("questionnaire "));
			text += theme.fg("muted", `${count} question${count !== 1 ? "s" : ""}`);
			if (labels) {
				text += theme.fg("dim", ` (${labels})`);
			}
			return new Text(text, 0, 0);
		},

		renderResult(result, _options, theme, _context) {
			const details = result.details as QuestionnaireResult | undefined;
			if (!details) {
				const text = result.content[0];
				return new Text(text?.type === "text" ? text.text : "", 0, 0);
			}
			if (details.cancelled) {
				return new Text(theme.fg("warning", "Cancelled"), 0, 0);
			}
			const lines = details.answers.map((a) => {
				if (a.wasCustom) {
					return `${theme.fg("success", "✓ ")}${theme.fg("accent", a.id)}: ${theme.fg("muted", "(wrote) ")}${a.label}`;
				}
				const display = a.index ? `${a.index}. ${a.label}` : a.label;
				return `${theme.fg("success", "✓ ")}${theme.fg("accent", a.id)}: ${display}`;
			});
			return new Text(lines.join("\n"), 0, 0);
		},
	});

	pi.registerTool({
		name: "question",
		label: "Question",
		description: "Ask one question and let the user choose an option. Use when work needs user input to continue.",
		parameters: QuestionParams,

		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const optionLabels = params.options.map((o) => o.label);
			const details = (answer: string | null, wasCustom?: boolean): QuestionDetails => ({
				question: params.question,
				options: optionLabels,
				answer,
				...(wasCustom === undefined ? {} : { wasCustom }),
			});

			if (ctx.mode !== "tui") {
				return textResult("Error: UI not available (running in non-interactive mode)", details(null));
			}
			if (params.options.length === 0) {
				return textResult("Error: No options provided", details(null));
			}

			const result = await runPicker(ctx, [
				{
					id: "question",
					label: "Question",
					prompt: params.question,
					options: params.options.map((o) => ({ value: o.label, label: o.label, description: o.description })),
					allowOther: true,
				},
			]);
			const answer = result.answers[0];

			if (result.cancelled || !answer) {
				return textResult("User cancelled the selection", details(null));
			}
			if (answer.wasCustom) {
				return textResult(`User wrote: ${answer.label}`, details(answer.label, true));
			}
			return textResult(`User selected: ${answer.index}. ${answer.label}`, details(answer.label, false));
		},

		renderCall(args, theme, _context) {
			let text = theme.fg("toolTitle", theme.bold("question ")) + theme.fg("muted", args.question);
			const opts = Array.isArray(args.options) ? args.options : [];
			if (opts.length) {
				const labels = opts.map((o: { label: string }) => o.label);
				const numbered = [...labels, OTHER_LABEL].map((o, i) => `${i + 1}. ${o}`);
				text += `\n${theme.fg("dim", `  Options: ${numbered.join(", ")}`)}`;
			}
			return new Text(text, 0, 0);
		},

		renderResult(result, _options, theme, _context) {
			const details = result.details as QuestionDetails | undefined;
			if (!details) {
				const text = result.content[0];
				return new Text(text?.type === "text" ? text.text : "", 0, 0);
			}
			if (details.answer === null) {
				return new Text(theme.fg("warning", "Cancelled"), 0, 0);
			}
			if (details.wasCustom) {
				return new Text(
					theme.fg("success", "✓ ") + theme.fg("muted", "(wrote) ") + theme.fg("accent", details.answer),
					0,
					0,
				);
			}
			const idx = details.options.indexOf(details.answer) + 1;
			const display = idx > 0 ? `${idx}. ${details.answer}` : details.answer;
			return new Text(theme.fg("success", "✓ ") + theme.fg("accent", display), 0, 0);
		},
	});
}
