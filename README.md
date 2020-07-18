# hascard
A commandline utility for reviewing notes. 'Flashcards' can be written in markdown-like syntax.

![a recording of example usage of the hascard application](./recordings/recording.gif)

## Cards
### Definition
This is the simplest card, it simply has a title and can be flipped to show the contents. For example the following card
```
# Word or question
Explanation or definition of this word, or the answer to the question.
```
will result in
![](./recordings/definition.gif)

### Multiple choice
This is a typical multiple choice question. The question starts with `# ` and the choices follow. Only one answer is correct, and is indicated by a `*`, the other questions are preceded by a `-`. As an example, the following text

```
# Multiple choice question, (only one answer is right)
- Choice 1
* Choice 2 (this is the correct answer)
- Choice 3
- Choice 4
```

gets rendered as
![](./recordings/multiple-choice.gif)

### Multiple answer
Multiple choice questions with multiple possible answers is also possible. Here again the question starts with `#` and the options follow. Preceding each option is a box `[ ]` that is filled with a `*` or a `x` if it is correct. For example

```
# Multiple answer question
[*] Option 1 (this is a correct answer)
[ ] Option 2
[*] Option 3 (this is a correct answer)
[ ] Option 4
```
results in
![](./recordings/multiple-answer.gif)

### Open question
Open questions are also supported. The words that have to be filled in should be surrounded by underscores `_`. Multiple answer possibilities can also be given by seperating them with vertical bars `|`. As an example, the card

```
# Fill in the gaps
The symbol € is for the currency named _Euro_, and is used in the _EU|European Union_.
```
behaves like this
![](./recordings/gapped-question.gif)

## Miscellaneous info
Written in Haskell, UI built with [brick](https://github.com/jtdaugherty/brick) and parsing of cards done with [parsec](https://github.com/haskell/parsec). Recordings of the terminal were made using [terminalizer](https://github.com/faressoft/terminalizer).