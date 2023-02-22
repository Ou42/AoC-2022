# My Advent of Code 2022 Journey!

## Using Haskell, running on Linux:

| TOOL            | VERSION | via                    |
| :---            | :------ | :--                    |
| GHCi            | 8.6.5   | ghci --version         |
| stack           | 2.9.3   | stack --version        |
| GHC under stack | 9.2.5   | stack ghc -- --version |

## Completed: 

| DAY   | Part A  | Part B  | ghci    | stack build |
| :---: | :---:   | :---:   | :--:    | :---:       |
| 01    | &check; | &check; | &cross; | _           |
| 02    | &check; | &check; | &cross; | _           |
| 03    | &check; | &check; | &cross; | _           |
| 04    | &check; | &check; | &cross; | _           |
| 05    | &check; | &check; | &cross; | _           |
| 06    | &check; |         | &cross; | _           |
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
