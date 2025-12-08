-- NvimURLHandler.applescript
-- Handles nvim:// URLs to open files in the correct Neovim instance
-- URL Format: nvim://file//path/to/file.txt:42?tmux-session=session-name

on open location schemeUrl
	set oldDelims to AppleScript's text item delimiters

	try
		-- Extract content after "nvim://"
		set AppleScript's text item delimiters to {"nvim://"}
		if (count of text items of schemeUrl) < 2 then error "Invalid URL format"
		set fullContent to item 2 of the text items of schemeUrl

		-- Extract file path after "file/"
		set AppleScript's text item delimiters to {"file/"}
		if (count of text items of fullContent) < 2 then error "Invalid URL format, missing file path"
		set pathWithQuery to item 2 of the text items of fullContent

		-- Split path from query string
		set AppleScript's text item delimiters to {"?"}
		set filePath to item 1 of the text items of pathWithQuery

		-- Extract tmux-session parameter if present
		set tmuxSession to ""
		if (count of text items of pathWithQuery) > 1 then
			set queryPart to item 2 of the text items of pathWithQuery
			set AppleScript's text item delimiters to {"tmux-session="}
			if (count of text items of queryPart) > 1 then
				set tmuxSession to item 2 of the text items of queryPart
				-- Handle multiple query params
				set AppleScript's text item delimiters to {"&"}
				set tmuxSession to item 1 of the text items of tmuxSession
			end if
		end if

		-- Call the handler script (symlinked to ~/.local/bin)
		do shell script "/bin/bash " & quoted form of ((system attribute "HOME") & "/.local/bin/nvim-url-handler") & " " & quoted form of tmuxSession & " " & quoted form of filePath

	on error errMsg
		display alert "Error processing nvim:// URL" message errMsg
	end try

	set AppleScript's text item delimiters to oldDelims
end open location
