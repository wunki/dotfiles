# Fast mode in Pi's footer

This local copy of [pi-openai-fast-mode 0.5.0](https://github.com/johncmunson/pi-openai-fast-mode) shows its live state beside the model and thinking level, instead of in a separate widget.

`/fast`, `/fast on`, `/fast off` and `--fast` work as before. The footer shows `fast off`, `fast on`, or `fast on (unavailable)` when the toggle is on but the selected model isn't targeted. Green means the plugin will request priority service; it doesn't confirm that the provider granted it.

`settings.json` loads this directory as a local package. The existing `config.json` and project-level `.pi/pi-openai-fast-mode/config.json` keep their meaning. The local target list adds `gpt-6.1-sol` for both `openai` and `openai-codex`; request payloads and persistence are unchanged from upstream. Startup and `/reload` refresh saved targets while preserving the on/off setting.

[OpenAI supports Fast mode for GPT-6.1 Sol](https://developers.openai.com/codex/speed). On Codex with ChatGPT sign-in, Fast mode consumes included subscription limits at 2.5× the Standard rate; purchased credits and Enterprise pay-as-you-go use 2×. The plugin requests `service_tier: "priority"`, which OpenAI also accepts as Fast mode.

The local behavior changes are in `src/config.ts` and `src/status.ts`; relative imports also carry `.ts` extensions. It decorates Pi's exported `FooterComponent.render` because Pi has no inline-footer slot. Shutdown and `/reload` restore the original renderer. Recheck this integration when upgrading Pi, especially the model row's position. When updating the plugin, compare with upstream and retain the local status renderer and GPT-6.1 Sol targets.

From the dotfiles root, with Pi installed:

```sh
PI_OFFLINE=1 PI_SKIP_VERSION_CHECK=1 node --test pi/agent/extensions/pi-openai-fast-mode/test/footer.test.mjs
```

The test uses Pi's real session, commands and footer, with an in-memory UI and no model requests. It covers GPT-6.1 Sol on both providers, saved-target refresh, toggles, model switches, request targeting, narrow widths, other footer rows, session-local state and cleanup.
