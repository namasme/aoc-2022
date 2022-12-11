# Advent of Code 2022
This is my set of solutions for the 2022 edition of Advent of Code, now in Haskell.

## Dependencies
You will need a working version of [Stack](https://github.com/commercialhaskell/stack) installed in your system to run this project. You can install it via [GHCup](https://www.haskell.org/ghcup/), which will manage installation of other Haskell utilities as well.

## Setup
See `setup.sh` for details on how to recreate the setup.

## Usage
#### Creating the files with the correct structure and downloading the input for a new day/problem:
``` sh
$ ./make-day.sh <day number>
```

To be able to download the input you will need to provide a session cookie, which is read from `.session`.

In order to get it, log in to [Advent of Code](https://adventofcode.com) in your browser, open the Developer Tools and go to the Network tab (you might need to refresh the page now). The request for the page itself (not the assets) should include your cookie in the headers, like so:

```
cookie: session=<your session cookie>
```

Copy that value in `.session` and you should be good to go. Shout-out to [smores56](https://github.com/smores56/aoc-2022#logging-in-to-advent-of-code) for the tip!

#### Running the solution for a given part (of a day's problems)
Open the appropriate file in Emacs, then type `C-c l` to load the code in a project-aware REPL (so that imports work).

This will use GHCi to execute the program, so for compute-intensive problems you may want to compile first to increase performance. In that case, simply edit `app/Main.hs` and set `main` to the `solution` of the part in question, then execute

``` sh
$ stack run
```


#### Running the tests for a given day
``` sh
$ ./test-day.sh <day number>
```

#### Running all the tests
``` sh
$ stack test
```


## TODOs
- [x] Bolster `make-day.sh` so that it automatically downloads the problem's input to the correct location (see e.g. https://github.com/smores56/aoc-2022/blob/main/prep-day.sh).
- [ ] Figure out a way to avoid having to change `app/Main.hs` when needing to compile the solution of a certain problem. Ideally the main executable should take two parameters that specify the day and part that has to be run, but the correct solution may be to create separate executables for each part. Needs investigation. 
