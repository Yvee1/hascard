# Changelog for hascard
## 0.2.1.0
New:
- A certain chunk of a deck of cards can be reviewed using the `-c i/n` CLI option. For example `hascard -c 3/5` will split the deck into 5 chunks, and review the 3rd one. These chunks can be used with the shuffle and amount options too.
- Visual indicator in the deck selector menu for whether the deck is being shuffled or not.
- Shuffling can be toggled inside the deck selector menu using the 's' key.
- Most parsing error messages now show up as a pop-up box with nicer formattting than before.
- The maximum number of recently selected decks stored and shown can now be changed in the settings menu.

Fixed bugs:
- Flickering when switching between menus is gone. This was done by merging the seperate Brick applications into one.

## 0.2.0.0
New:
- A new type of card is available: reorder the elements. This could break previous definition or open question cards if they had the same format as the new reorder the elements card. In that case change the 1. 2. etc. to something like 1), 2) which is not seen as a reorder type card.
- For open question cards, if you don't know the answer you can click F1 which shows the correct answer

Fixed bugs:
- Hinted definitions were not accurate because the carriage return character \r was not seen as whitespace, but as content

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
