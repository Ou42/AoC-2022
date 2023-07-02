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
  - DFS WIP

## 2023-03-01

- Part A
  - DFS working, can now traverse the "Zipper"
  - To Do carried fwd: keep track of folder sizes

## 2023-03-04

### Day 07

- Part A
  - Not sure if my traversal was a DFS
  - Could be that it was a "preorder traversal"  
    to create the directory tree output
  - but then appeared to need a "postorder taraversal"  
    to store the sizes
  - So, I'd need to traverse the tree 2x
  - For now, I'm going to table that.
  - New alg: Using a `Data.Map`
  - `parse` is my new alg
  - Using `Data.Map.insert`, `adjust` & `lookup`
  - `Data.Map.lookup` returns a Maybe !!
  - using `Data.Maybe.fromJust` to extract Int
  - there might be a better/more robust way!
  - SUCCESS!!
- Part B
  - Started ...
  - Noticed I wasn't moving back to root
  - therefore dir totals weren't accurate in Part A!
  - luck that it didn't matter for Part A, but it will for Part B
  - created `backToRoot` to `cd ..` until back at root
  - Now file system total(s) are accurate for Part B
  - finding the smallest folder >= amount need to be freed
  - where `amtReqToFree = 30 * 10^6 - (70 * 10^6 - dirSizesTotal)`
  - sorting the candidates & filtering puts the answer at the head
  - SUCCESS!!
  - TO-DO: Code cleanup ...
  
## 2023-03-21

### Part A

- Solved!
- Feels a bit heavy computationally.
  
### Part B

- Started
- WIP
- having trouble visualizing how to implement my "alg"
- copy/pasted code and removed "computation" and returned original input
- this proved passing in `id` & `flip` to `visL2R` is working
- got it!
- not the most efficient solution, but fast enough!
- would be interesting to keep track of "max" visibility along the way

## 2023-04-12

### Day-09 - Part A

- Started
- doOneStep => one iteration of a MoveInstruction
- updateLoc => given a MoveDir, update a Loc
- better grasp of movement
  - Tail can be diagonally away from Head
  - but not by more than 1 unit in either dimension
  - Only need to check if `mustMove` on "first step"
  - subsequent steps can be precalculated

## 2023-04-13

### Day-09 - Part A

- refactored to use Sets not Maps
  - the keys are important, no need for a value
  - and still don't want duplicates
- Using a Record to hold/pass fwd Head & Tail Loc's
  - in addition to storing Tails history in `visited`
- `NamedFieldPuns` LANGUAGE pragma
- `doOneStep` is now `doFirstStep`
- added `mustMove` to check if Tail should move
- added `doFullMove` which *will* do a full Move Instruction
- `scanl` FTW?!

## 2023-04-14

### Day-09 - Part A

- `scanl` does work!
- But, ***My premise/assumption is wrong!!***
- It **IS** possible that T won't move for 2 steps of a Move Instr
- So, now is it only the first 2 moves? or 3?
- It currently seems that T might not move on steps 1 and 2
- But it must start following H on step 3
- Also, once T starts following H, it continues to do so for the  
  remainder of the Move Instruction
- changed doFirstStep to doStepWithCheck
- SUCCESS!
- code is a mess, needs clean-up / refactor
- went w/ *simpler* less "optimal"?! alg
- maybe my "optimization" isn't that much of performance boost?!

## 2023-04-27

### Day-09 - Part B

- started Part B
- cleaned up some of the code

## 2023-05-02

### Day-09 - Part B

- pair coded with OFD
- WIP
- see: `day-09-B.hs`

## 2023-05-06

### Day-09 - Part B

- using `Knot` data type from pair coding session
- tweaking it a bit, added constructor `PrevKnotPrevPos Pos`
- in *this* version, not including `Dir` in `Knot` constructor
- `doStepWithCheckB` split into: `moveHeadKnot` & `moveTails`
- `moveHeadKnot` still takes a `MoveDir`
- `moveTails` doesn't need it
- `mustMoveTail` helped *reduce* pattern matching code
- probably need to repeat that elsewhere
- need to test: `moveHeadKnot`, `moveTails` & `oneMovePartB`
- `oneMovePartB` is one "step" that ripples thru the [Knot]
- onced that is (proven) working, need:
  - a "full" move func
  - an "all" moves func

## 2023-05-08

### Day-09 - Part B

- test `oneMovePartB` = **PASS** &check;
- test `iterate` `oneMovePartB` (10x) = **FAIL!**
  - wasn't reducing `[Knot]`
  - interesting that it still terminated!
- hlint suggestions:
  - remove redundant brackets
  - use `_` instead of `otherwise`
  - `print ...` instead of `putStrLn $ show ...`
  - instead of: `let moveR10 = head $ drop 9 $ iterate (oneMovePartB 'R') moveRecB`  
    use: `let moveR10 = iterate (oneMovePartB 'R') moveRecB !! 10`
- `doFullMoveB` & `doAllMovesB` ... but there's a bug!
  - ... produces from answer.
- refactored code from input from John during FP MU
  - simplify
  - point free leads to code reduction (func removal in some cases)
  - when there's no real need for `flip` swap the order of the args and remove it

## 2023-05-09

### Day-09 - Part B

- further refactor based on John's suggestions ( FP MU )
- `calcNewPos` vs the *wrong* intuition of previous Knot's previous Pos
- using `clamp` to get the row & col offset amt to move the tail knot's Pos
- Day-09 - Part B **SOLVED!**

## 2023-07-01

### Day-09

- cleaned up my git/GitHub "mess". ***TYVM, PMD!!***
- created a new branch, `day-09-with-OFD`
- `git rm day-09-B.hs` from `day-09` branch
- ( had to commit for it to "show" on that branch only. )
- day-09-B.hs is the WIP alt solution, pair coded with OFD
- left that file in the new branch
- did a PR on the `day-09` branch
- merged it into `main` branch -- 19 commits!

### Day-10

- started
- for a quick start, modeled the input as `[Maybe Int]`
- parsing input done
