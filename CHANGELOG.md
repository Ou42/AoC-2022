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

## 2023-01-02

- renamed README.md to CHANGELOG.md
- created new README.md
  - with tables! So fancy!

### Day 23 continued ...

- fixed formatting in days/23/timings.md
- fixed formatting of the the "fixed formatting" ...
- Part B using HashSets &check;
- timings updated
- don't need Main.hs
- and the executable is now smaller! 1.89 MB v 4.89 MB
- but it doesn't copy the exe to the root folder

## 2023-01-03

### Day 23 continued ...

- \$ `stack build` followed by:  
  \$ `stack exec <exe file>` runs the file
- \$ `stack build --copy-bins` copies exe to the "local-bin-path"
- \$ `stack path` lists all paths stack is potentially using
- `local-bin-path` defaults to ~/.local/bin
- `copy-bins` & `local-bin-path` can be set in <stack.yaml>
- now copies exe to project root
- disabling ghc-options: `threaded`, `rtsopts` & `with-rtsopts=-N`  
  in <package.yaml> ***improved*** performance!  
  e.g. for Part B using HashSet, 15s v 23s
- and shrank exe from 1.89 MB to 1.82 MB

## 2023-01-06

### Day 23 continued ...

- version 1.x `notUsingSetPartA`, using same `partA` func,  
  but passing in: `id id foldr (:) [] elem`
- it's *slower* than "native" List version (ver 1)?!
- Data.Bifunctor (bimap)  
  `bimap minmax minmax $ unzip coords`
- adding tables for timings data in timings.md WIP

## 2023-01-08

### Day 23 continued ...

- Part B table for timings data in timings.md

## 2023-02-18

### Day 04

- Part A success!
  - `stack install split`
  - `stack ghci --package split`
  - `:l day-04.hs`
  - `main`
- Part B success!
  - same instructions as Part A

## 2023-02-20

### Day 05

- Part A started
  - parsing container "stacks" into lists
  - parsing move instructions
  - applying move op WIP getting "Pattern match is redundant"
  - SUCCESS! ( created `applyMove` and then used `foldl` )
- Part B
  - started
  - SUCCESS!
  - fortunately I didn't *literally* move 1 box at a time!
  - I was able to edit `applyMove` to accept a function so 
    that it'd work for both Part A & B ... attempt at DRY.

## 2023-02-22

### Day 06

- Part A
  - started
  - SUCCESS! simple / naive solution
  - *could* jump ahead when pairs or triplets aren't unique
  - *could* check to make sure app doesn't crash if marker not found
- Part B
  - started
  - SUCCESS! simple /naive solution (again)
  - did refactor Part A's code to be useful for both A & B
  - DRY principle

## 2023-02-25

### Day 07

- Part A
  - started
  - start of parsing via pattern matching
  - more parsing
  - parsing appears to be working
  - To Do: keep track of folder sizes
  - To Do: traverse the "Zipper"
  - `fsToRoot` moves up to the root of the Zipper
