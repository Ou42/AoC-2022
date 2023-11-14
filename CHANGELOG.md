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

## 2023-07-02

### Day-10

- created, `cmdsToRegXHist` function
- using the test data provided
  - it works for the 20th, 60th, 100th, 140th, & 180th cycles!
  - it is off by one on the 220th cycle ?!
- there may be a *different* off by one error. '*During* cycle N...'
- to test that theory, I output regX's History cycles 19..21, etc
- it *does* look like I am off by one!

---

- correct answer from the test data
- off by one! lookup cycle 19, 59, 99 ...
- SUCCESS! with puzzle data!

## 2023-07-07

### Day-10 - Part B

- started
- some tweaks to `updateCRT` but still not working
- created my own `chunksOf` to split the rows
- don't call `unlines` until *after* done splitting!
- changed signature of `chunksOf` to be more generic
- can now send it `[a]` ... `[Int]` and not just `String`
- note: `unlines` only works on `[String]` ( `[[Char]]` )

## 2023-07-08

### Day-10 - Part B

- SUCCESS!
- I *forgot* what regXHist contained
  - added them to an accumulator (**wrong!**)
  - they **are** the *value* of the sprite's location!
- tweaked `updateCRT` ( removed the func sig )
  - to be able to see what was happening ( sprite pos )

---

## Day-11 - Part A

### 2023-07-09

- started

### 2023-07-14

- parsing input file started

### 2023-07-15

- MultiWayIf FTW - WIP
- `map $ foldl` over `monkeyStrings`
- changing gears: attempting Parser Combinators
  - read: <https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html>
  - started
  - so far, only the Monkey's ID is read and converted to an `Int`
  - next up: how to read a variable number of CSV `Int`'s?
  - before that: quick refactor thanks to hlint:
    - use `<$>` instead of `fmap`
    - use `isDigit` instead of checking if the char is in a range
    - drop the lambda
  - Bing-Chat FTW -- asked how to parse CSV ( List of `Int`'s )
    - but it only works when they are *only* separated by a ','
    - not go when separated by ", " ( comma then a space )
    - fixed with `... sepBy string ", "` vs prev `... sepBy char ','`
  - replaced my `parseWhiteSpace` with lib func `skipSpaces`
  - a tiny bit more refactoring

### 2023-07-16

- attempted to use Bing-Chat's solution for reading the "Operation" (`parseExpr`)
- how to *test* the function?!
- NOTE: when trying to skip some parsers, then the next parser will fail!!

### 2023-07-17

- quick test instructions
- parseTest WIP
- parseTest working!
- `[ReadPMonkey]` parsed
- converted to a `Map` with MokeyID (`rpmID`) as key

### 2023-07-19

- `makeMonkeysMap` fixed
- `doOp` WIP
  - it *should* handle one monkey's item list
  - maybe break it down further & handle one item?
- `doOp` renamed to `doOneOp`
- created a list with updated Monkeys
- used `M.union` to update MonkeysMap
- rpInspected !!! must keep tally of how many items a Monkey inspected
- created a bug in `doOneRound`
  - When folding over the MonkeyMap the Monkey being sent in is the *original* Monkey
  - ***not*** the updated Monkey
  - so its Item list ( worry list ) is stale
- `doOneRound` fixed! iterates over keys, not values (Monkeys)
- `doOneOp` changed to work with keys, not values (Monkeys)
- `PartA` success!
- minor code cleanup
  - removed by hand file parsing WIP dead code

## Day-11 - Part B

### 2023-07-20

- started
- worry level no longer lowered via (`div` 3)
- 10,000 rounds instead of 20
- my result /= theirs
- suspect `Int` isn't sufficient
- attempting to convert to using `Integer`
  - but it's slower
  - and my implementation has a space-leak(s)
  - ... and the OS will enventually Kill it
- I've tried to prove Integer is required
  - from the straight, calculations are going > than maxBound :: Int, yes!
  - perhaps some math-magic can allow use of `Int`, but I don't see it (yet)
- I've tried `foldr` and `foldl'` in `do_10K_RoundsPartB`
  - interestingly, the`foldr` ver runs for a long time before being Killed
  - the `foldl'` ver *slowly* spits out the trace!!
 - simply put, this alg is too slow. What am I missing?
 
### 2023-07-21

- I cheated. Pfffft.
- I was ( and still am?! ) missing ... modulo arithmetic
  - <https://www.reddit.com/r/adventofcode/comments/14zr6uo/2022_day_11_part_2/>
  - <https://chasingdings.com/2022/12/11/advent-of-code-day-11-monkey-in-the-middle/>
- apparently, taking the mod or `rem` of a magic number is OK
- in this case the magic number is the LCM of all the test numbers
- This number is different per data set
- For the test data-set it is `(13*17*19*23)`
- What I can't wrap my mind around is how this still works with addition
- Again, using the **test data-set**
  - Monkey 1: has the op: `new = old + 6`
  - Monkey 3: has the op: `new = old + 3`
  - how can taking the `rem` work in those cases?!
- started alt modulo arithemtic ver
  - changed `parseTest` func
  - it now *returns* `(testNum, testFunc)`
  - nope, I'm wrong. It ***has*** to be a mulitple of all the `rpTextNum`'s
- SUCCESS!
  - calculated `rpTestNumsMult` == Multiple of all `rpTestNum`'s
  - left in `trace` code in `doOneOpPartB`
- code cleanup

## Day-12 - Part A

### 2023-07-24

- started
- instructions

### 2023-08-02

- $ cabal update
- $ cabal install linear
- but probably only needed vector:
- $ cabal install vector
- `import Data.Vector (Vector)`
- `import qualified Data.Vector as V`
- `type Array2D = Vector (Vector Char)`

```
Warning:
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ WARNING: Installation might not be completed as desired! @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
The command "cabal install [TARGETS]" doesn't expose libraries.
* You might have wanted to add them as dependencies to your package. In this
case add "vector" to the build-depends field(s) of your package's .cabal file.
* You might have wanted to add them to a GHC environment. In this case use
"cabal install --lib vector". The "--lib" flag is provisional: see
https://github.com/haskell/cabal/issues/6481 for more information.
```

- $ cabal install --lib vector
- Data.Vector vs Data.Array ?!
- sticking to Data.Vector for now
- `import qualified Data.Vector as V`
- (V.!?) :: Vector a -> Int -> Maybe a
- got help w/ pointfree
- pointfree.io was too agressive. Wish it had steps
- *Was* able to ask Bing to keep `row` and `col`
- `getElevation row col arr2d = (arr2d V.!? row) >>= (V.!? col)`  
  becomes:  
  `getElevation row col = (V.!? row) >=> (V.!? col)`

### 2023-08-08

- `hasAvailableNextStep` WIP! -- needs to deal w/ 'S' & 'E'
- `deadEnds` also WIP. Just checking if unused locations
- `imap` provide index & elem
- shortest path algorithm(s) !!
  - Dijkstra
  - A*
  - can a recursive traversal work?
- implemented `findStart` & `findEnd`
- imported `Map` to keep track of visited locations

### 2023-08-09

- `findPath` WIP
  - `validMoves`
  - `isValidElevation` -- checks if (<= succ currElev) && not in visited Set
  - `possNextSteps`
- instead of `Int -> Int` changed to `(Int, Int)`
  - `(row, col)`
- backtracking?!
  - currently, it *should* stop when no available `validMoves`
- it worked! it found a path from Start to End!
  - but it might have extra steps due to how I'm adding them?!
- fixed the returned path by prepending `end`

```
        if currPos == end
          then  end:solutionPath
```

- definitely has extra steps:

```
> fi <- readFile "input-12.txt"
> solution = findPath fi
> length solution
6547
> s = S.fromList solution
> S.size s
4872
```

### 2023-08-10

- after asking Bing AI for help w/ BFS
- and following a couple links ( Brilliant & a SO thread )
- I tweaked `findPath` and made `findShortestPathDistance`
- it works for the "test data" but not the "real data"
- The bug has to do with adding duplicate nodes to `todo` List
- `todo` should be like a Set. No duplicates!
- Part A Solved!
  - bug was solved by doing a `union` op: ``nextDepthNodes `union` validNextSteps``
- Part B Started

### 2023-08-11

- Part B - start at any 'a' elevation square ( or 'S' ), what's the shortest path to the goal?
- `findAllInArray` looks like it's working -- returns `[(row, col)]` of all starting squares
- semi-brute force alg: run them all, return the min distance
  - found a potential "bug" from Part-A:  
    if this altered BFS runs out of legal moves and doesn't find the goal,  
    it shouldn't return `depth`, but maxBound :: Int so as to not get chosen as a solution
  - also, to boost performance a bit, include all starting sq's in visited!?
  - correction: all, but the current starting square!
- Part B Solved!
  - but w/o the optimization
  - I'd still like to attempt the optimization and see if it is actually faster
- started work on semi-brute force alg perf boost:
  - the following was to check `visited` was correct
  - in the following, `prepopulatedVistedPartB` returned:  
        `map (\start -> (start, visited start)) allStartSqrs`

```haskell
    ghci> fi <- readFile "input-12-test.txt" 
    ghci> prepopulatedVisitedPartB fi
    [((0,0),fromList [(0,1),(1,0),(2,0),(3,0),(4,0)])
    ,((0,1),fromList [(0,0),(1,0),(2,0),(3,0),(4,0)])
    ,((1,0),fromList [(0,0),(0,1),(2,0),(3,0),(4,0)])
    ,((2,0),fromList [(0,0),(0,1),(1,0),(3,0),(4,0)])
    ,((3,0),fromList [(0,0),(0,1),(1,0),(2,0),(4,0)])
    ,((4,0),fromList [(0,0),(0,1),(1,0),(2,0),(3,0)])
    ]
```

- `prepopulatedVisitedPartB` updated to use correct `visited`

### 2023-09-09

- Day-12 Profiling *WIP*
- see: /days/12/README-aoc-2022-day-12.md

## Day-13 - Part A

### 2023-11-09

- started
- renamed test input files: `input-XX-test.txt` to `input-XX.test`
  - now they are saved to the repo
  - days 08-12
  - consider editing the .hs files to ack this change

### 2023-11-10 - 2023-11-13

- simple WIP parse of the input data
  - `(Packet "[1,1,3,1,1]",Packet "[1,1,5,1,1]")`
  - `(Packet "[[1],[2,3,4]]",Packet "[[1],4]")`
- enumerated the list of `Pairs`:
  - `Pairs 1 (Packet "[1,1,3,1,1]",Packet "[1,1,5,1,1]")`
  - `Pairs 2 (Packet "[[1],[2,3,4]]",Packet "[[1],4]")`
- `parsePacket`
  - started WIP
  - now handles 10's ( or any multi-digit Int )
- `Show` instance / parsing cleanup
  - parsing done?!
  - does `show` an extra set of [ ]'s surrounding the Packet
- `cmpPairs` WIP
  - works for test input data Pairs 1, 7, & 8
  - instead of return `Bool`, returning `Pair #` if True and 0 if False?!
    ... so it can be `Sum`ed easily at the end
- switching to recursive data structure
  - [x] parsing | `parsePacket'` -- good enough?!
    ```haskell
    -- 0 | [1,1,5,1,1]
    -- 3 | [[1],[2,3,4]]
    ghci> parsePacket' $ lns !! 0
    [Nested [Val' 1,Val' 1,Val' 3,Val' 1,Val' 1]]
    ghci> parsePacket' $ lns !! 3
    [Nested [Nested [Val' 1]],Nested [Val' 2,Val' 3,Val' 4]]  
    ```
  - [x] parsing | `parsePacket'` *improved!*
      ```haskell
      ghci> parsePacket' $ lns !! 0
      Packet' [Val' 1,Val' 1,Val' 3,Val' 1,Val' 1]
      ghci> parsePacket' $ lns !! 3
      Packet' [Nested [Val' 1],Nested [Val' 2,Val' 3,Val' 4]]
      ```
- `parseInput'`
- it compiles! it runs! ... it doesn't produce the right answer!
- bug hunt!
  - refactor, but nothing discovered
  - `disp2` to convert (display) the parsed data back in original form
  - need to check if anything was parsed incorrectly
  - yes, yes it was ( parsed incorrectly):
    ```haskell
    Pair 1: [[[6,10],[4,3,[4]]]], ... -- prettyPrint - raw input data
    Pair 1: [[[6,10]],[4,3,[4]]], ... -- disp2       - parsed & converted back

    Packet [Nested [Nested [Val 6,Val 10]],Nested [Val 4,Val 3,Nested [Val 4]]]

    -- easier test:
    ghci> a = "[[[6,10],[4,3,[4]]]]"
    ghci> dispPacket $ parseP2 a
    "[[],[6,10],[4,3],[4]]"
    ```
  - It's taken me a while to see this, but `span (/= ']')` is the culprit!
    - Instead of the *first* ']', it should include everthing up to the matching ']'
    - How to *find* that matching ']'?!
    - To help in testing, created the *simpler* helper func: `parseP3`
      ```haskell
      ghci> parseP3
      [[[6,10],[4,3,[4]]]] source
      [[[6,10]],[4,3,[4]]] converted/reverted
      Match: False
      ```
  - tried other ways with recursion & an accumulator. Closer, but not quite there.
  - asked Bing for more help, but it kept writing bad code.
    - It'd compile, but wouldn't create the correct structure
  - Then I asked Bing for tutorials!
    - here's one, [Write_Yourself_a_Scheme_in_48_Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing)
  - `parseP2c` FTW?! uses `splitP` to extract matching `[ ]` List
    - refactor 1: `parseP2c` renamed `parseP2cTest_1` and `go` renamed `parseP2c`
    - refactor 2: more renaming, `parseP2c` now is `String -> PacketList`
- *SUCCESS!!*
  - dead code removal
  - wish 1: find a recursive parse solution
  - wish 2: use a parser combinator solution: `readP`?!
  - wish 3: fix clunkiness / extra pattern matching etc

### 2023-11-14

- Day-13 - Part A | Wish 1: Find a recursive parse solution WIP
