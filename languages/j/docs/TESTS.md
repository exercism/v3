If you run in to any issues with running the tests at all please just submit your solution and a mentor will help you figure out what's wrong. J is a language that few people use, so finding information can be brutal.


# TLDR

-   Head to the problem directory (eg `~/Exercism/hello-world`).
-   Start a `jconsole` session (eg by typing `jconsole` or `ijconsole`) in a terminal.
-   Load your solution file (eg `load 'hello-world.ijs'`)
-   Test your solution by typing `0!:2 < 'test.ijs'`.


# Terminal

The way the tracks tests are set up is hoped to be as simple as possible. If you're working an exercise, say `hamming.ijs` here's how the work should flow. Edit `hamming.ijs` writing out your solution. Interactively build it up by first typing

```j
NB. your j terminal binary may be ijconsole (eg. on Debian)
jconsole 'hamming.ijs' NB. start a J console session with definitions loaded hamming.ijs
```

Then from inside the J console, reload by typing

```j
load 'hamming.ijs'
```

To run the tests &#x2013; after you have loaded your solution &#x2013; type

```j
0!:2 < 'test.ijs'
```

The verb (aka function) [0!:2](https://code.jsoftware.com/wiki/Vocabulary/Foreigns#m0) is an example of something called a *foreign*. It loads its argument and treats each line of it as an assertion which will be true or false (1 or 0) and prints out the line and the result of checking the assertion. Another option to test is using `0!:3` which won't print for each line but will return 1 if all assertions pass or 0 if something doesn't. `0!:2` and `0!:3` expect boxed strings as their arguments, that's the purpose of `<` above.
