import Gio from 'gi://Gio';
import Meta from 'gi://Meta';
import Shell from 'gi://Shell';

import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';
import * as Main from 'resource:///org/gnome/shell/ui/main.js';

const APPLICATION_SHORTCUTS = [
    {
        settingsKey: 'raise-or-launch-ghostty',
        desktopId: 'com.mitchellh.ghostty.desktop',
    },
    {
        settingsKey: 'raise-or-launch-firefox',
        desktopId: 'firefox.desktop',
    },
];

const LEGACY_GHOSTTY_SHORTCUT = {
    schemaId: 'org.gnome.shell.keybindings',
    settingsKey: 'switch-to-application-2',
    accelerator: '<Primary><Alt>t',
};

export default class ApplicationShortcutsExtension extends Extension {
    enable() {
        this._removeLegacyGhosttyShortcut();
        this._settings = this.getSettings();

        for (const {settingsKey, desktopId} of APPLICATION_SHORTCUTS) {
            Main.wm.addKeybinding(
                settingsKey,
                this._settings,
                Meta.KeyBindingFlags.IGNORE_AUTOREPEAT,
                Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
                () => this._raiseOrLaunch(desktopId)
            );
        }
    }

    disable() {
        for (const {settingsKey} of APPLICATION_SHORTCUTS)
            global.display.remove_keybinding(settingsKey);

        this._settings = null;
    }

    _raiseOrLaunch(desktopId) {
        const application = Shell.AppSystem.get_default().lookup_app(desktopId);
        if (!application) {
            console.warn(`Application not found: ${desktopId}`);
            return;
        }

        const [window] = application
            .get_windows()
            .filter(candidate => !candidate.skip_taskbar);

        if (!window) {
            application.activate();
            return;
        }

        Main.overview.hide();
        Main.activateWindow(window, global.get_current_time());
    }

    _removeLegacyGhosttyShortcut() {
        // Version 1 stored Ctrl+Alt+T on GNOME's second favorite-app shortcut.
        // Remove only that accelerator, preserving Super+2 and user additions.
        const {schemaId, settingsKey, accelerator} = LEGACY_GHOSTTY_SHORTCUT;
        const settings = new Gio.Settings({schema_id: schemaId});
        const accelerators = settings.get_strv(settingsKey);
        const currentAccelerators = accelerators.filter(
            candidate => candidate !== accelerator
        );

        if (currentAccelerators.length !== accelerators.length)
            settings.set_strv(settingsKey, currentAccelerators);
    }
}
