# My Advent of Code 2022 Journey!

## Using Haskell, running on Linux:

| TOOL            | VERSION | via                    | notes               |
| :---            | :------ | :--                    | :----               |
| GHCi            | 8.8.4   | ghci --version         | Day 01-08, 23       |
|                 | 9.2.7   |                        | Day 09 - Part B ... |
| stack           | 2.9.3   | stack --version        |                     |
| GHC under stack | 9.2.5   | stack ghc -- --version |                     |

## Completed: 

| DAY | Part A  | Part B  | ghci    | build tool | profiling |
| :-: | :-----: | :-----: | :-----: | :--------: | :-------: |
| 01  | &check; | &check; | &check; | _          | _         |
| 02  | &check; | &check; | &check; | _          | _         |
| 03  | &check; | &check; | &check; | _          | _         |
| 04  | &check; | &check; | &check; | _          | _         |
| 05  | &check; | &check; | &check; | _          | _         |
| 06  | &check; | &check; | &check; | _          | _         |
| 07  | &check; | &check; | &check; | _          | _         |
| 08  | &check; | &check; | &check; | _          | _         |
| 09  | &check; | &check; | &check; | _          | _         |
| 10  | &check; | &check; | &check; | _          | _         |
| 11  | &check; | &check; | &check; | _          | _         |
| 12  | &check; | &check; | &check; | `cabal`    | &check;   |
| 13  | &check; | _       | _       | _          | _         |
| 23  | &check; | &check; | &check; | `stack`    | &check;   |

## Run / Build & Run Instructions:

```text
NOTE: copy & save input data into days/XX/input-XX.txt

$ cd days/XX

then:

$ ghci
λ :l day-XX.hs
λ main

-- or --

$ stack build
$ stack exec dayXX-exe

-- or --

$ stack build
$ ./dayXX-exe

-- or --

$ cabal run <pkg name> && hp2ps -e8in -c <pkg name>.hp
```
