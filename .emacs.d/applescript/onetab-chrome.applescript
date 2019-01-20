set title_string to ""
tell application "Google Chrome"
	set window_list to every window
	repeat with the_window in window_list
		set tab_list to every tab in the_window
		repeat with the_tab in tab_list
			set the_title to the title of the_tab
			set clean_title to my replace(the_title, "[", "|")
			set clean_title to my replace(clean_title, "]", "|")
			set the_url to the URL of the_tab
			set title_string to title_string & "  * [[" & the_url & "][" & clean_title & "]]" & return
		end repeat
	end repeat
end tell

# delay 1
#
# tell application "Google Chrome"
# 	set window_list to every window
# 	repeat with the_window in window_list
# 		set tab_list to every tab in the_window
# 		repeat with the_tab in tab_list
# 			close the_tab
# 		end repeat
# 	end repeat
# end tell


on replace(theText, theSearchString, theReplacementString)
	set AppleScript's text item delimiters to theSearchString
	set theTextItems to every text item of theText
	set AppleScript's text item delimiters to theReplacementString
	set theText to theTextItems as string
	set AppleScript's text item delimiters to ""
	return theText
end replace
