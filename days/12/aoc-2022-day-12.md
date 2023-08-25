# Profiling in Haskell Adventure

## 2023-08-24

- new setup, following: <https://nikita-volkov.github.io/profiling-cabal-projects/>
- `$ cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks`
- in this case, copied .cabal file from test run
  - used settings from link above
  - updated ghc-options that were deprecated
  - added `-prof`
  - changed: `-h` to `-hT`
  - left: `i0.1` alone as I figured it was erroring out due to previous errors
- `$ cabal install --only-dependencies --enable-library-profiling`
- `$ cabal run day12-aoc22 && hp2ps -e8in -c day12-aoc22.hp`

```
Warning: 'ghc-options: -prof' is not necessary and will lead to problems when
used on a library. Use the configure flag --enable-library-profiling and/or
--enable-profiling.
```

- but it ran!

```
------------------------------------------
Start = (20,0)
End   = (20,138)
------------------------------------------
Part A
------
Shortest Path Distance = 440
    ( for test input (see comments above), the answer is 31 )
    for real test data, the answer will vary dending on data set
------------------------------------------
Part B
------
Shortest Path Distance = 439
    ( for test input (see comments above), the answer is 29 )
    for real test data, the answer will vary dending on data set
------------------------------------------
prepopulatedVisitedPartB
------
Shortest Path Distance = 439
    ( for test input (see comments above), the answer is 29 )
    for real test data, the answer will vary dending on data set
   5,810,561,528 bytes allocated in the heap
     220,952,464 bytes copied during GC
       1,099,192 bytes maximum residency (119 sample(s))
         144,904 bytes maximum slop
              22 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1290 colls,  1290 par    1.019s   1.039s     0.0008s    0.0095s
  Gen  1       119 colls,   118 par    0.337s   0.193s     0.0016s    0.0051s

  Parallel GC work balance: 17.24% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.004s  (  0.003s elapsed)
  MUT     time    9.655s  (  9.376s elapsed)
  GC      time    1.305s  (  1.181s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.051s  (  0.050s elapsed)
  EXIT    time    0.002s  (  0.001s elapsed)
  Total   time   11.016s  ( 10.611s elapsed)

  Alloc rate    601,825,515 bytes per MUT second

  Productivity  88.1% of total user, 88.8% of total elapsed
```

- I think `day12-aoc22.ps` is squished because the `vector` pkg name is too long

---

## 2023-08-23

```
$ ghc -O2 --make day-12.hs -prof -auto-all -caf-all -fforce-recomp
Loaded package environment from /home/me/.ghc/x86_64-linux-9.2.7/environments/default

on the commandline: warning: [-Wdeprecated-flags]
    -auto-all is deprecated: Use -fprof-auto instead

on the commandline: warning: [-Wdeprecated-flags]
    -caf-all is deprecated: Use -fprof-cafs instead
[1 of 1] Compiling Main             ( day-12.hs, day-12.o )

day-12.hs:65:1: error:
    Could not find module ‘Data.Vector’
    Perhaps you haven't installed the profiling libraries for package ‘vector-0.13.0.0’?
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
65 | import Data.Vector (Vector)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^

day-12.hs:66:1: error:
    Could not find module ‘Data.Vector’
    Perhaps you haven't installed the profiling libraries for package ‘vector-0.13.0.0’?
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
66 | import qualified Data.Vector as V
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

---

```
$ ghc-pkg list | grep vector 
me at me-Ame-A10 in ~/cs/aoc/AoC-2022/days/12 on day-12-profiling
$ cabal update && cabal install vector 
Downloading the latest package list from hackage.haskell.org
Updated package list of hackage.haskell.org to the index-state 2023-08-24T00:43:23Z
To revert to previous state run:
    cabal v2-update 'hackage.haskell.org,2023-08-08T19:56:09Z'
Resolving dependencies...
Up to date
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

---

but, `$ ghc -O2 day-12.hs` compiles just fine!

---

```
$ ghc -O2 --make day-12.hs -prof -fprof-auto -fprof-cafs -fforce-recomp
Loaded package environment from /home/me/.ghc/x86_64-linux-9.2.7/environments/default
[1 of 1] Compiling Main             ( day-12.hs, day-12.o )

day-12.hs:65:1: error:
    Could not find module ‘Data.Vector’
    Perhaps you haven't installed the profiling libraries for package ‘vector-0.13.0.0’?
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
65 | import Data.Vector (Vector)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^

day-12.hs:66:1: error:
    Could not find module ‘Data.Vector’
    Perhaps you haven't installed the profiling libraries for package ‘vector-0.13.0.0’?
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
66 | import qualified Data.Vector as V
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

---

`$ cabal init`

---

`$ cabal build`

---

```
$ cd days/12
$ cabal run
Up to date
Hello, Haskell!
```

