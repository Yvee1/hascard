# Changelog for hascard
## 0.1.4.0
New:
- Deleted or moved files are also no longer shown in the recently selected files list

Fixed bugs:
- A new line after the last seperator `---` caused the parsing to fail, now it does not
- If the last card was an open question and there was no `---` at the end, it was shown as a definition card (issue #6, thanks @stemvork)

## 0.1.3.0
New:
- Add `-s` shuffle flag and `-a` amount option to CLI.
- Add macOS binary and homebrew formula

## 0.1.2.0
New:
- Hidden files/directories (so starting with '.') are now hidden by default, and can be shown by pressing 'h' in the filebrowser


Fixed bugs:
- Parsing now succeeds even if the text file does not end with ---
- When passing a file to the CLI, the absolute path is saved instead of the relative one, preventing issues with the 'recently selected files'

## 0.1.1.0
New:
- Add nix build support (by @srid)
- Recently used decks are now ordered by most recent first
- Recently used decks now have unique names. Previously only the filename was shown, but now path is shown up to unique name

Fixed bugs:
- Failed parsing of settings file now results in default settings instead of error
- After selecting a file from system, the card selector had to be refreshed before the file appeared in the recently selected decks list. The file is now present immediately
- Directly selecting a card via the CLI now also adds it to the recents list
- Selecting a card via the CLI now returns an error if it has a different filetype than '.txt'. If no filetype is given, '.txt' is assumed.


## 0.1.0.0
Initial release
