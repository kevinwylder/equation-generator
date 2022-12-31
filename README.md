# Equation Generator

This program generates all valid equations of a fixed character width, for a simple arithmatic language. Equations look like this

```
1+1=2
4*5=20
8*4-2=30
291/3=97
...
```

The language allows for `+-*` and `/`, along with integer constants. This is a basic calculator syntax, based on nerdlegame.com

## Language details

* Multiplication and Division are evaluated first, going from left to right
* Addition and subtraction are evaluated second, also going left to right
* Fractions do not exist. If two numbers do not evenly divide, the expression is skipped
    * This feels wrong because `3/4=6/8` is obviously true
* While you can create a negative expression using subtraction (like `1-8`) negative constant literals are not allowed (`-7`)
