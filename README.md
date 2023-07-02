# My Advent of Code 2022 Journey!

## Using Haskell, running on Linux:

| TOOL            | VERSION | via                    | notes            |
| :---            | :------ | :--                    | :----            |
| GHCi            | 8.8.4   | ghci --version         | Day 01-08, 23    |
|                 | 9.2.7   |                        | Day 09 - Part B  |
| stack           | 2.9.3   | stack --version        |                  |
| GHC under stack | 9.2.5   | stack ghc -- --version |                  |

## Completed: 

| DAY   | Part A  | Part B  | ghci    | stack build |
| :---: | :---:   | :---:   | :--:    | :---:       |
| 01    | &check; | &check; | &cross; | _           |
| 02    | &check; | &check; | &cross; | _           |
| 03    | &check; | &check; | &cross; | _           |
| 04    | &check; | &check; | &cross; | _           |
| 05    | &check; | &check; | &cross; | _           |
| 06    | &check; | &check; | &cross; | _           |
| 07    | &check; | &check; | &cross; | _           |
| 08    | &check; | &check; | &cross; | _           |
| 09    | &check; | &check; | &cross; | _           |
| 10    |  WIP    |         |         | _           |
| 23    | &check; | &check; | &cross; | &cross;     |

## Run / Build & Run Instructions:

```text
NOTE: copy & save input data into days/XX/input-XX.txt

$ cd days/XX
$ ghci
λ :l day-XX.hs
λ main

-- or --

$ cd days/XX
$ stack build
$ stack exec dayXX-exe

-- or --

$ cd days/XX
$ stack build
$ ./dayXX-exe
```
