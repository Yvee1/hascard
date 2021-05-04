# Changelog for hascard
##
New:
- Setting for changing case sensitivity of open questions. Default behaviour is case sensitive, which was also the functionality before this setting was added.

Fixed bug:
- When the open question was gotten right after the first try, it was still counted as correct in review mode

## 0.5.0.1
Fixed bugs:
- When generating new decks from correct or incorrect cards, 'Nothing' was added as the first line
- When a gap was the last part of a sentence, the newline was not shown correctly

## 0.5.0.0
New:
- References to files can be added inside cards, such that the corresponding files opens when encountering that card. The file opens in a different window using the default application for opening files of that file type configured on your computer. This can be used to add images to questions. The syntax is described [here](https://github.com/Yvee1/hascard#including-files) in the readme.
- LaTeX can be included in a card, which is first converted to a PDF and then opened with a PDF viewer. More info [here](https://github.com/Yvee1/hascard#latex) in the readme.

UI updates to parameter selection:
- Selecting the chunks in the parameter selection window is better
- The deck size visible in the parameter selection window now correctly works together with the chunk option; i.e. the maximum number of cards represents the number of cards in the selected chunk, instead of the total number of cards in the entire deck.

Some other UI updates, thanks to @g-w1:
- When entering text in an answer Control-w will go back a word
- In multiple choice, tab will also toggle the option
- In the flashcard, the "yes or no" prompt can be navigated by 'l' and 'h' for a more vi like experience
- Quitting can be done with 'q' in addition to 'ESC'

## 0.4.0.0
New:
- UI menu for setting the parameters like shuffling etc. The CLI options are no longer usable with `hascard`. The CLI options have been moved under `hascard run`. Directly providing a file is now also done with `hascard run`.
- Convert TSV files to files compatible with hascard, using the `hascard import` command. (suggested by @g-w1)

Fixed bugs:
- Focus cycling removed in settings menu for consistency with the other menus.
- Better error for non-existing files
- Allow | character in text
- Allow lists with - in definition cards. Previously this was seen as a multiple choice question without any correct answers, and therefore gave an error.

## 0.3.0.1
Fixed bugs:
- Crash on empty recents list
- Crash on any key other than Enter at FinalPopup

## 0.3.0.0
New:
- When reviewing a deck, the amount of correctly and incorrectly answered cards are now displayed.
- After answering a definition card a popup appears asking whether you answered it correctly or not.
- After finishing a deck, there is an option to create new decks from either the correctly answered or incorrectly answered cards, or both. The correct cards of a file named `deck.txt` are stored in `deck+.txt` in the same folder, and the incorrect ones in the file `deck-.txt`. Make sure you do not have files of those names that you want to keep since these _will_ be overwritten.
- The `--blank` or `-b` option was added to use the application without the changes listed above.
- Files with the `.md` extension are also accepted by the application and shown in the filebrowser. 

Fixed bugs:
- Empty open question now takes up 1 row instead of 0 rows, so does not jump like before.
- All parse errors now show a popup instead of crashing the application when reviewing a deck.

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
