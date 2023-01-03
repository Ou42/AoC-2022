# TIMINGS

## 2023-01-01

### ghci:
---

```text
*Sets λ main
------ Part A version 2 using Set:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 61041638936000 end = 65952756094000 Time = 4911117158000
         ... or 4911.117158 ms
------------------------
------ Part A version 3 using HashSet:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 65953546719000 end = 70649083565000 Time = 4695536846000
         ... or 4695.536846 ms
------------------------
------ Part B version 2 using Set:
Rounds until no movement = 988
Start = 70650115062000 end = 363622715568000 Time = 292972600506000
         ... or 292972.600506 ms
------------------------
```

### compiled via `stack build`:
---

```text
$ ./Main
------ Part A version 1:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 3984807000 end = 29991316704000 Time = 29987331897000
	 ... or 29987.331897 ms
------------------------
------ Part A version 2 using Set:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 29991616471000 end = 30343731669000 Time = 352115198000
	 ... or 352.115198 ms
------------------------
------ Part A version 3 using HashSet:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 30343866336000 end = 30651515723000 Time = 307649387000
	 ... or 307.649387 ms
------------------------
------ Part B version 2 using Set:
Rounds until no movement = 988
Start = 30651661289000 end = 50297581126000 Time = 19645919837000
	 ... or 19645.919837 ms
------------------------
```

## 2023-01-02

### ghci:
---

```text
*Sets λ main
------ Part A version 2 using Set:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 1784155179000 end = 9456780096000 Time = 7672624917000
         ... or 7672.624917 ms
------------------------
------ Part A version 3 using HashSet:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 9458654005000 end = 15938060114000 Time = 6479406109000
         ... or 6479.406109 ms
------------------------
------ Part B version 2 using Set:
Rounds until no movement = 988
Start = 15939031412000 end = 326014717084000 Time = 310075685672000
         ... or 310075.685672 ms
------------------------
------ Part B version 3 using HashSet:
Rounds until no movement = 988
Start = 326016021490000 end = 617906670208000 Time = 291890648718000
         ... or 291890.648718 ms
```

### compiled via `stack build`:
---

```text
$ ./Main
------ Part A version 1:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 3505534000 end = 30536001787000 Time = 30532496253000
	 ... or 30532.496253 ms
------------------------
------ Part A version 2 using Set:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 30536085786000 end = 30948562411000 Time = 412476625000
	 ... or 412.476625 ms
------------------------
------ Part A version 3 using HashSet:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 30948612603000 end = 31278762426000 Time = 330149823000
	 ... or 330.149823 ms
------------------------
------ Part B version 2 using Set:
Rounds until no movement = 988
Start = 31278824893000 end = 57379249190000 Time = 26100424297000
	 ... or 26100.424297 ms
------------------------
------ Part B version 3 using HashSet:
Rounds until no movement = 988
Start = 57379316686000 end = 76146094365000 Time = 18766777679000
	 ... or 18766.777679 ms
```

## 2023-01-03

### ghci via `stack ghci`:
---

```text
*Main Paths_Day23 Sets λ main
------ Part A version 1:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 2962213342000 end = 42881205376000 Time = 39918992034000
	 ... or 39918.992034 ms
------------------------
------ Part A version 2 using Set:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 42882182305000 end = 52866163811000 Time = 9983981506000
	 ... or 9983.981506 ms
------------------------
------ Part A version 3 using HashSet:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 52867646266000 end = 59392776780000 Time = 6525130514000
	 ... or 6525.130514 ms
------------------------
------ Part B version 2 using Set:
Rounds until no movement = 988
Start = 59393480229000 end = 378985902503000 Time = 319592422274000
	 ... or 319592.422274 ms
------------------------
------ Part B version 3 using HashSet:
Rounds until no movement = 988
Start = 378987389498000 end = 665841741362000 Time = 286854351864000
	 ... or 286854.351864 ms
```

### compiled via `stack build`:
---

```text
$ ./day23-exe 
------ Part A version 1:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 3435323000 end = 22343817885000 Time = 22340382562000
	 ... or 22340.382562 ms
------------------------
------ Part A version 2 using Set:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 22344051055000 end = 22709039044000 Time = 364987989000
	 ... or 364.987989 ms
------------------------
------ Part A version 3 using HashSet:
minmax X = -5 75
minmax Y = -4 75
6480 (The minimum orthogonal rectangular area)
minus 2563 (the # of elves)
======
3917 (The answer for Part A)
Start = 22709218677000 end = 23011996053000 Time = 302777376000
	 ... or 302.777376 ms
------------------------
------ Part B version 2 using Set:
Rounds until no movement = 988
Start = 23012197435000 end = 43686024220000 Time = 20673826785000
	 ... or 20673.826785 ms
------------------------
------ Part B version 3 using HashSet:
Rounds until no movement = 988
Start = 43686074725000 end = 58387430644000 Time = 14701355919000
	 ... or 14701.355919 ms
```
