# Day 09 NOTES

A few things I looked into while working on *Day 09 - Part B*

- `foldl`
- `foldr`   -- for short-circuiting
- `foldl'`  -- strict `foldl`
- `scanl'`
  - strict `scanl`
  - "returns a list of successive reduced values from the left"
  - per: <https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-List.html#g:6>

Pair coded with *oldfartdeveloper*

- naming things is hard!
- rather than `Loc` we came up w/ `Knot`
  - and, considered `Pos` for *position*
  - rather than `Loc` for *location*
- considered `headKnot` and `tailKnot`
- but also, different *data constructors*
  - `data Knot = HeadKnot Pos Dir | TailKnot Pos`
  - or maybe, `prevKnotPos` and `currKnotPos`
  - ... which could be *pattern matched* on

Also considering ...

- using `sequence` or `array` for faster access to the last elem
- *might* not be necessary if I roll my own `scanl'`
