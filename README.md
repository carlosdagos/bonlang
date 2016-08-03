Bonlang
=======

Stylised as, though this is subject to change, ...

```
  __
 |__)  _   _  |  _   _   _
 |__) (_) | ) | (_| | ) (_)
                        _/
```

## Introduction

It's a programming language! A minimalist programming language.

I wrote (most) this during the ZuriHac 2016 weekend, and **it's still a work in
progress.**

## First steps

Head over to the [examples](test/examples/) folder for some good stuff.
Basically, your basic `hello-world` looks like this:

```
/*
 * file: main.bl
 *
 * This is where I tell you things
 */

module Main where

def main [] = {
    print $ "Hello World!";
}
```

You need to always have a `Main` module with a `main` method defined. This
is what the runtime looks for in order to start the program.

## Other examples

### Lazy evaluation

The language is lazy, so this yields "Lazy Hello" but not "Hello World"

```
module Main where

def main [] = {
    val sayHello = puts-ln $ "Hello World!";
    puts-ln $ "Lazy Hello!";
}
```

Those curly braces indicate "Instruction blocks". The runtime will try go
through semicolon-separated expressions, reducing where necessary.

So this **will** produce output

```
module Main where

def main [] = {
    val print' = print $ "Hello World!";
    print'; // Needs evaluation to happen
}
```

### Automatic currying

The language supports automatic currying:

```
module Main where

def addValues [x, y] = + $ x y

def add2 [y] = addValues $ 2

def main [] = {
    puts-ln $ (add2 $ 2); // Will output '4'
}
```

### Function application

Function application is done with the `$` operator after an expression. If
the expression is a function, it will be applied. The runtime will throw an
error otherwise.

You should be familiar with Polish notation.

### Lambda support

The language supports lambdas (closures)

```
module Main where

/**
 * Will output '4' twice in new lines
 */
def main [] = {
    val λ1 = lambda [x, y] => + $ x y;
    val λ2 = lambda [y] => λ1 $ 2 y;
    puts-ln $ (λ1 $ 2 2) (λ2 $ 2);
}
```

## Motivations

This is developed in Haskell, I wanted to
test [`stack`](https://docs.haskellstack.org/en/stable/README/)
and [`parsec`](https://hackage.haskell.org/package/parsec),
and that eventually turned into this.

I first read (and actually this contains code from)
[Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).
That prompted me to design this. It has elements from Haskell, Scala, and Lisp.

It's fun.

## Brought to you by

Carlos D'Agostino.

You can visit my blog here https://cdagostino.io

[Disclaimer](doc/images/noidea.png).

[About my fixation with the number 4](https://xkcd.com/221/).

## LICENSE

See [LICENSE](LICENSE) file in this repo.

## TODOS

See [TODO](TODO.md) file in this repo.
