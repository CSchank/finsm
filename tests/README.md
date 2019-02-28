# Testing

This README will serve as notes, and possibly documentation later on,
on testing `finsm`.

## Notes

### Testing ideas
- Oracle testing: Use JS's inbuilt `regex` library, wrapped on top of
  Elm's `core/regex` interface, as ground truth.
- From there we can do one of the following things:
  1. Create a regular expression to NFA program. Testing can commerce by
     converting a user-provided/randomly generated regular expression
     to an NFA using `Machine.elm`'s machine type, then checking equality
     with the oracle via (randomly generated) unit tests
  2. Create a NFA to regular expression program. Testing can commerce by
     converting a user-provided machine to an equivalent regular expression,
     which we can feed into the oracle.
  3. Other suggestions welcome!
- We can try testing an equivalence class of regular expressions: For every
  randomly generated regular expression, we can apply term rewriting rules
  of Kleene Algebra to simplify the expression, then we can test that
  they accept the same regular language when converting both expressions
  into machines. This has the advantage of not being reliant on an oracle,
  although we can certainly include the oracle as well.

### Considerations
- Should part/all of this be written in another functional language or proof checker?
  E.g. Haskell, Agda?
- Current approach favors unit testing, but can we employ property-based testing?
  E.g. with Fuzz, QuickCheck? What properties can we/should we check?
- Are there parts of the program we can do a formal proof on? E.g. epsilon transitions?
- Features used in testing can be made available to the app for users as well! E.g.
  the conversions to/from machine and regex

## Possible References
- Kozen, D. C. (2012). Automata and computability. Springer Science & Business Media.
