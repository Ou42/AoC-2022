# AoC-2022

## 2022-12-19

### Start of my Advent of Code 2022 Journey!

- .gitignore .txt files ==> input files
- *any* notes should be in .md or here

### Day 01 - Part A

- start of day-01
- build up tiny functions
- test along the way
- first bug: "/n/n" instead of "\n\n"

### Day 01 - Part B

- sortBy in Data.List not Data.Sort?!
- reverse sort using (flip compare)
- there are other ways: "Down" ?!

### Day 02 - Part A

- start of day-02
- simple solution to Part A

### Day 02 - Part B

- start on day-02 Part B
- ugly solution to Part B

## 2022-12-20

### Day 03 - Part A

- WIP
- ~~Data.Text?~~
- ~~Data.ByteString?~~ another time!
- Used Data.Set to find the duplicate item
- Used Data.List for the item to Int conversion

## 2022-12-22

### Day 03 - Part B

- Data.Set -> find intersection between 3 groups

## 2022-12-24

### Day 23 - Part A

- Skipping ahead to Day 23!
- How to find the minmax in one pass?
- found: <https://hackage.haskell.org/package/foldl>
- How to find the minmax of the fst and snd of a tuple in one pass?
- But first ... parse the input data!
- WIP: putStrLn $ unlines $ map show es
- parsing works! 2D "Array" -> List of 2D coordinates
- WIP: helper funcs for doing a round
- WIP: doRound func

## 2022-12-25

### Day 23 - Part A

- doRound: want a single Map, not a List of Maps
- Q: how to move the elves and then rotate the move order & then
     move the elves again?

## 2022-12-26

### Day 23 - Part A

- Answer: use a fold!
- but also need doRound to return the new positions & new move order
- Part A - SUCCESS!
- but a tad slow
- ... and Part B requires checking to see if no elves moved  
  which will take more time to calculate.
- other points to ponder: foldl' vs foldr
- foldr can short-circuit
- foldl' can't, but might be fast(er)

### Day 23 - Part B

- for Part B return number of rounds until no movement
- I'm thinking foldr & short-circuiting
- weirdness with foldr, but progress
- lazy evaluation issue? res/acc aren't updating
- some optimization attempts. Less code, but not faster!?

## 2022-12-27

### Day 23 - Part B ( and A )

- It's too slow. Perhaps not converting back to a List ea round?
- Prep for Part A Version 2: Using Sets
- code cleanup:
  - renamed some funcs
  - now passing funcs to funcs
  - DRY
  - future use: made timing diff vers easier
- vscode tip: Ctrl + ` (backtick) toggles Terminal  
  ( can leave ghci running! )
- Ver 2 using sets: WIP
  - WIP on doRndV02UsingSet
  - canMoveUsingSet vs canMove  
    Set.member O(log n) vs List.elem O(???)

## 2022-12-28

### Day 23 - Part B ( and A )

- Part B Solved!
  - Part A Version 2, using Sets is much faster
  - Much of Part A is used in Part B
  - Solved Part B in 260889.949274 ms
  - but ... "every problem has a solution that completes  
    in at most 15 seconds on ten-year-old hardware."  
    per: <https://adventofcode.com/2022/about>
  - What am I missing?!

## 2022-12-29

### Day 23 - Part A ( but for B )

- Data.HashSet - faster, but not by much.
- Perhaps Data.IntSet with my own from/to (x,y) hash funcs?
- Or maybe a way to determine what won't move?
- Or maybe memoize precomputed values?

## 2022-12-31

### Day 23 - Part A

- refactor -> sets.hs
- can send in a Set or HashSet ...
- prep for Data.IntSet  
  ... but will need to/from key helper funcs

## 2023-01-01

- continue refactor -> Sets.hs
- renamed sets.hs to Sets.hs
- setup stack tool to compile via `stack build`
- compiled ver finishes **MUCH** faster!  
  Part B using Sets: 290s vs 20s !!  
  see also: days/23/timings.md
- To-Do:
  - Part B using HashSets
  - Data.IntSet
