//allow for 'contrib' stuff
load_paths.unshift("chrome://conkeror-contrib/content/");

// my custom keys
define_key(text_keymap, 'C-w', 'cmd_deleteWordBackward');

// improve hint keys
hint_background_color = "transparent";
hint_digits = "aoeuhtns";

// load the mono theme
theme_load("mono");

// Don't require a whitelist to install extensions
session_pref("xpinstall.whitelist.required", false);

// Adblock plus
require("adblockplus.js");

// firefox user agent
session_pref("general.useragent.compatMode.firefox", true);

// don't enable formfill
session_pref("browser.formfill.enable", false);

// default directory
cwd = make_file("/home/wunki/downloads");

// Save my passwords
session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");
Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager);

register_user_stylesheet(
    "data:text/css," +
	escape(
	  "span.__conkeror_hint {" +
		" font-size: 10px !important;" +
		" line-height: 12px !important;" +
    " opacity: 0.9 !important;" +
    " padding: 2px !important;" +
    " -moz-border-radius: 2px;" +
    " font-weight: normal !important;" +
		"}"));

// hint's color
register_user_stylesheet(
    "data:text/css," +
	escape (
	  "span.__conkeror_hint {" +
		" border: 1px solid black !important;" +
		" color: #333 !important;" +
		" background-color: yellow !important;" +
		"}"));

// teach me something whenever I start my browser
homepage = "about:blank";

// open clicks in buffers (tabs) in the background
require("clicks-in-new-buffer.js");
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; 
clicks_in_new_buffer_button = 1;

// instapaper
require("/home/wunki/src/wunki-dotfiles/instapaper.js");

// auto completion in the minibuffer
minibuffer_auto_complete_default = true;
url_completion_use_history = true;
url_completion_use_bookmarks = true;

// open new urls in new buffer
url_remoting_fn = load_url_in_new_buffer;

// modify the modeline
require("mode-line.js");
require("mode-line-buttons.js");

// we'd like to see the # of buffers being loaded 
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);

// who wants a clock?
remove_hook("mode_line_hook", mode_line_adder(clock_widget));

// smart links
define_webjump("gs", "https://encrypted.google.com/?q=%s");

// reload conkerorrc with C-c r
interactive("reload-config", "reload conkerorrc",
       function(I) {
          load_rc();
          I.window.minibuffer.message("config reloaded");
       }
);
define_key(default_global_keymap, "C-c r", "reload-config");

// org-protocol stuff
function org_capture (url, title, selection, window) {
    var cmd_str = 'emacsclient \"org-protocol:/capture:/w/'+url+'/'+title+'/'+selection+'\"';
    if (window != null) {
      window.minibuffer.message('Issuing ' + cmd_str);
    }
    shell_command_blind(cmd_str);
}

interactive("org-capture", "Clip url, title, and selection to capture via org-protocol",
          function (I) {
              org_capture(encodeURIComponent(I.buffer.display_uri_string),
                          encodeURIComponent(I.buffer.document.title),
                          encodeURIComponent(I.buffer.top_frame.getSelection()),
                          I.window);
          });

// capture with C-c c
define_key(content_buffer_normal_keymap, "C-c c", "org-capture");

// clear history
function history_clear () {
    var history = Cc["@mozilla.org/browser/nav-history-service;1"]
        .getService(Ci.nsIBrowserHistory);
    history.removeAllPages();
};
interactive("history-clear", "Clear the history.", history_clear);
