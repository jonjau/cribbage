## Cribbage

This Prolog module contains predicates that score and select score-maximising
hands in the [game of Cribbage](https://en.wikipedia.org/wiki/Cribbage).

The main predicates are:

- `hand_value(+Hand, +Startcard, ?Value)`:
   Evaluates a 5-card cribbage hand `[Startcard|Hand]`, as in the show.
- `select_hand(+Cards, +Hand, ?Cribcards)`:
   Selects cards to keep in the hand (and to discard to the crib),
   to maximise expected hand value when the start card is revealed,
   by averaging the scores across all possible card arrangements

This was a submission for Project 1 of COMP30020 at the University of
Melbourne, Semester 2 2020. The grade received was 29.0/30.0, with feedback
about factorials and binomial coefficients being overkill for `score_pairs/2`.

## Running in the interpreter

1. Have [SWI-Prolog](https://www.swi-prolog.org/) installed.
2. Run `swipl` in the terminal.
3. Load the module with `[cribbage].`, then call the predicates.

Tests are in [`tests.pl`](./tests.pl). Running that in the interpreter should
return `true`, if all tests pass.
