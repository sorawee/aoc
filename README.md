# aoc-2019

My framework and solutions for Advent of Code 2019.

## How to run

```
racket runner.rkt [--day <day>] [--year <year>] <mode> 
```

where `<mode>` is either `new`, `download`, ~~`submit-1`, `submit-2`~~, `test`,  or any task name.

For every command, if `--year` is not specified, it defaults to the current year. Similarly, if `--day` is not specified, it defaults to the current day *if* the contest of the current year is still active and the year is the current year. Note that a year starts at December, and a day starts at 1 hour before each contest.

### `new`

The `new` mode creates a directory and a stencil file.

### `download`

The `download` mode downloads an input. You need to provide a session number if you have not already provided it. 

### `submit-1` and `submit-2`

(Not yet implemented) Both modes submit an answer. Note that if you have already `submit-1` on a day and passed, you won't be able to run this command again. Similarly, if you have not already run `submit-1` on a day, you won't be able to run `submit-2`. You need to provide a session number if you have not already provided it. 

### `test`

The `test` mode runs all tests. 

## Note on my solution

I don't care about ranking in this challenge. I'm more interested in writing
code with a relatively good quality (though I won't bother putting too much 
effort into writing docs).
