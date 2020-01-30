# hascard
An in-progress commandline utility for reviewing notes. 'Flashcards' can be written in markdown-like syntax. Different kind of questions are possible:
- [x] Simple flashcard
- Multiple choice question
  - [x] Single correct answer
  - [ ] Multiple correct answers
- [x] Fill in the gaps

### TODO:
- [ ] Spaced repetition (with Leitner system)
- [ ] Saving paths of previously used cards and other user data with xdg user directory

![a terminal application with a multiple choice question depicted on it](./scrot.png)

UI built with [brick](https://github.com/jtdaugherty/brick) and parsing of cards done with [parsec](https://github.com/haskell/parsec)
