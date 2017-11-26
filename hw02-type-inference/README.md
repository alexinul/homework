# PLDA - homework 2

In this assignment you'll have to take your definition of the language from the [first assignment](../hw01-interpreter/README.md) and write a type checker for your program that not only tells whether or not your program is correct, but it also infers the types. Given that we have only two types in the language, integers, and functions, this ought to be quite straightforward.

## Deadline

Sunday, December 17th 2017, midnight.

## The language AST:

Literally the same as in [homework 01](../hw01-interpreter/README.md). With the same freedom to change it.

## What is to be done

When in doubt of how to do it, simply emulate the behavior of the `:t` command in the [Haskell repl](http://learnyouahaskell.com/starting-out#ready-set-go).

Examples:
```
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :t 1 + 1
1 + 1 :: Num a => a
```

Given an expression in the language, `:t` will print out the type of the result, or give a type error. Of course, our language does not have the typeclass `Num` :).

```
Prelude> :t \x -> x
\x -> x :: t -> t
```

What is the type of the [identity](https://stackoverflow.com/questions/3136338/uses-for-haskell-id-function) function? Well, the repl is quite clear, it's a function type from any type `t` with a result value of type `t`.

#### Stating the obvious

Obviously, similarly to how you needed to represent "values" in your program using some specialized type, now you need a novel type representing types.

## Semantics
* `<binary-operation>`
  * the types of both expressions have to be of the "Integer" types. The resulting type is always "Integer"
* `<eq>`
  * the types of both expressions will be "Integer", and the result type as well
* `<if>`
  * unlike in the first homework, we cannot have different result types on both branches of the if — unless you implement union types from the "Extra Work" section. Therefore, it is a type error if both branches return something different.
* `<val-decl>`
  * work the same way as before. The type of the variable is inferred from its definition.
* `<val>`
  * semantically this boils down to "using" the value of a variable
  * fails at runtime if the variable that is being dereferenced is not defined anywhere
  * has the type of the variable inferred at definition call site
* <lambda>
  * the type of a lambda is always dependent on the number and types of its parameters, and the type of the resulting value
* `<apply>`
  * this, is in essence, has the type of the function you are applying

## Extra work

#### Optional type annotations

What you can do beyond the minimal work is adding the notion of explicit type annotations to your language (which definitely require changing your AST). Example of the all-knowing Haskell REPL:
```
Prelude> :t \x -> (x :: Integer)
\x -> (x :: Integer) :: Integer -> Integer
```
I explicitly state that the value returned by the function is `Integer`, therefore the type-checker knows to specialise the final type of my program.

#### Union types

This one is rather tricky. Neither Haskell, nor Scala have this notion; but Dotty — sometime to become Scala v3 — does [have them](http://dotty.epfl.ch/docs/reference/union-types.html).  But essentially it boils down to the idea that a type can be a union of multiple types, and the resulting value ascribed to one of those types.

Essentially, in a fictional language, it would look like:
```
> :t if (someCondition) 42 else \x -> x
Integer | (t -> t)
```

## Recomended reading

Unfortunately, none of it is in Scala, because most of the time, these things are expressed in terser languages. But there are full fledged implementations of type-inference for languages just as complicated as our own. It ought to be a minimal effort to understand these implementations even if they are written in all sorts things you're unfamiliar with.

------------------------------------------------------------------------------
* [Essentials of Programming Languages](https://karczmarczuk.users.greyc.fr/TEACH/Doc/EssProgLan.pdf), Friedman et. al. Chapter 7. — contains literally a step by step implementation — in Scheme — with plenty of explanations. Honestly, I could not explain it much better than this book, except maybe bring it into Scala-land.
* *Programming Languages: Application and Interpretation*, Shriram Krishnamurthi, [chapter 15](http://cs.brown.edu/courses/cs173/2012/book/types.html), specifically related to inference is [subsection 15.3.2](http://cs.brown.edu/courses/cs173/2012/book/types.html#(part._.Type_.Inference))
* implementations (in Ocaml) from the *Type and Programming Languages* book by Benjamin C. Pierce are available [online](http://www.cis.upenn.edu/~bcpierce/tapl/). You can access this book in [draft form](http://ropas.snu.ac.kr/~kwang/520/pierce_book.pdf) for free.
  * a Haskell port of the implementations from the TAPL book are also [available](https://code.google.com/p/tapl-haskell/)  
