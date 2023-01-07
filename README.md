# Nerdle Solver

This project helps solve [nerdle puzzles](https://nerdlegame.com/) - which is a mathematical take on wordle. It consists of 3 general components to do so

* a constraint matrix to narrow down the set of possible solutions
* a calculator syntax parser and iterator of all correct equations
* an entropy calculator, based on Grant Sanderson's [wordle video](https://www.youtube.com/watch?v=v68zYyaEmEA)

There are several ways to use this repo, outlined in this README

## The website

Hosted at https://nerdle.kwylder.com - this is the easiest way to cheat. After typing in an answer (`48-32=16` is the default first guess, but you can 
change it with delete) input the color sequence the puzzle returned to you and press enter to filter out the remaining answers. If there are fewer than
1,000 possible answers, it will categorize the guesses from best to worst based on the expected bits of information guessing would produce. This website
is basically on "hard mode" because better guesses may exist but will not be suggested if they can't be an answer.

Source for the website is in the `website` submodule

## The binary

In the root of this repo, `cargo run --release` will run `src/main.rs` (if you're new to rust, install the toolchain at https://rustup.rs/). The binary 
gives you access to more features than the website, like changing the equation width, solving binerdle puzzle constraints, suggesting answers that aren't 
potential solutions, and using multiprocessing to get better performance.

## Python bindings

Coming soon! This whole project is an excuse to learn rust and py bindings are a cool feature.
