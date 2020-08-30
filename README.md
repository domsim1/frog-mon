# frog-mon 🐸

Frogmon is a virtual pet game that lives in your terminal.
Frogmon works like a command line tool in the sense that you
care-for and observe a frogmon by passing arguments.

Made for fun and to improve in Haskell!

## how to play

To start, create a frogmon by running `frog-mon new`.

After you create a frogmon you can observe it by running
`frog-mon check`, this will show you an overview of how the
frogmon is doing. Run `frog-mon -h` or `frog-mon --help` to
see other commands you can preform to care for the frogmon.

A Frogmons status changes overtime time, do make sure to check
perodicly to ensure your froggy friend lives a long and happy
life.

## commands

|Command|Action|
|-------|------|
|-v, --version|Show current from-mon version|
|punish       |Use to discipline a frogmon when it is yelling 🗯|
|clean        |Use to fill Hygiene bar|
|heal         |Use to cure a sick frogmon 🦠|
|feed         |Use to fill Hunger bar|
|play         |Use to fill Enjoyment bar|
|new          |Create a new frogmon, overwriting the old save|
|check        |Shows an overview of current frogmon, also runs if no command is provided|
