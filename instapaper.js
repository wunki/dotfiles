// started from
// http://www.ceondo.com/ecte/2010/09/productivity-conkeror-instapaper-kindle
// Loïc d'Anterroches, Sep 7, 2010

interactive("instapaper", "Send the current page to InstaPaper.",
            function (I) {
                check_buffer(I.buffer, content_buffer);
		            var login_info = get_instapaper_password();
                let posturl = 'https://www.instapaper.com/api/add?' +
                    'username=' + login_info.username + '&' +
                    'password=' + login_info.password + '&url=' +
                    encodeURIComponent(I.window.content.location.href) +
                    '&selection=' +
                    encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "Description (optional): ",
			                      $history = "instapaper-description"
			                  ));
                try {
                    var content = yield send_http_request(load_spec({uri: posturl}));
                    if (content.responseText == "201") {
                        I.window.minibuffer.message("InstaPaper ok!");
                    } else {
                        I.window.minibuffer.message("Error.");
                    }
                } catch (e) { 
                    I.window.minibuffer.message("Error.");
                }
            });

interactive("instapaper-link", "Send the current link to InstaPaper.",
            function (I) {
                bo = yield read_browser_object(I) ;
                mylink = load_spec_uri_string(load_spec(encodeURIComponent(bo)));
                check_buffer(I.buffer, content_buffer);
	              var login_info = get_instapaper_password();
                let posturl = 'https://www.instapaper.com/api/add?' +
                    'username=' + login_info.username + '&' +
                    'password=' + login_info.password + '&url=' + mylink +
                    '&title=' + encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "Title (optional): ",
				                    $history = "instapaper-title",
                            $initial_value = bo.textContent)) +
                    '&selection=' + encodeURIComponent(
                        yield I.minibuffer.read(
                            $prompt = "Description (optional): ",
				                    $history = "instapaper-description",
                            $initial_value = "From: "+ I.buffer.title +" ("+I.window.content.location.href+")"
                        ));
                try {
                    var content = yield send_http_request(load_spec({uri: posturl}));
                    if (content.responseText == "201") {
                        I.window.minibuffer.message("InstaPaper ok!");
                    } else {
                        I.window.minibuffer.message("Error.");
                    }
                } catch (e) { 
                    I.window.minibuffer.message("Error.");
                }
            }, $browser_object = browser_object_links);

define_key(default_global_keymap, "C-x i", "instapaper");
define_key(default_global_keymap, "C-x I", "instapaper-link");

// Get the password
function get_instapaper_password() {
    var myLoginManager = Components.classes["@mozilla.org/login-manager;1"].getService(Components.interfaces.nsILoginManager);
    // dug these values out of .conkeror.mozdev.org/conkeror/*.default/signons.sqlite with sqlitebrowser
    var logins = myLoginManager.findLogins({}, 
					                                 "http://www.instapaper.com", // hostname
					                                 "http://www.instapaper.com", // formSubmitURL
					                                 ""); // realm
    // this is just for me, so don't bother search for usernames
    return { "username": logins[0].username, "password": logins[0].password };
}
