-- Send nvim:// file URLs to the Neovim process for a tmux session.
-- Format: nvim://file//path/to/file.txt:42?tmux-session=session-name

on open location schemeUrl
	set oldDelims to AppleScript's text item delimiters

	try
		-- Remove the URL scheme.
		set AppleScript's text item delimiters to {"nvim://"}
		if (count of text items of schemeUrl) < 2 then error "Invalid URL format"
		set fullContent to item 2 of the text items of schemeUrl

		-- Read the path after file/.
		set AppleScript's text item delimiters to {"file/"}
		if (count of text items of fullContent) < 2 then error "Invalid URL format, missing file path"
		set pathWithQuery to item 2 of the text items of fullContent

		-- Separate the file path from query parameters.
		set AppleScript's text item delimiters to {"?"}
		set filePath to item 1 of the text items of pathWithQuery

		-- Read tmux-session when present.
		set tmuxSession to ""
		if (count of text items of pathWithQuery) > 1 then
			set queryPart to item 2 of the text items of pathWithQuery
			set AppleScript's text item delimiters to {"tmux-session="}
			if (count of text items of queryPart) > 1 then
				set tmuxSession to item 2 of the text items of queryPart
				-- Ignore parameters that follow tmux-session.
				set AppleScript's text item delimiters to {"&"}
				set tmuxSession to item 1 of the text items of tmuxSession
			end if
		end if

		-- The Makefile links the shell handler into ~/.local/bin.
		do shell script "/bin/bash " & quoted form of ((system attribute "HOME") & "/.local/bin/nvim-url-handler") & " " & quoted form of tmuxSession & " " & quoted form of filePath

	on error errMsg
		display alert "Error processing nvim:// URL" message errMsg
	end try

	set AppleScript's text item delimiters to oldDelims
end open location
