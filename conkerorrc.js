//allow for 'contrib' stuff
load_paths.unshift("chrome://conkeror-contrib/content/");

// my custom keys
define_key(text_keymap, 'C-w', 'cmd_deleteWordBackward');

// teach me something whenever I start my browser
homepage = "about:blank";

// open clicks in buffers (tabs) in the background
require("clicks-in-new-buffer.js");
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; 
clicks_in_new_buffer_button = 1;

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
define_webjump("gs",    "https://encrypted.google.com/?q=%s");

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
