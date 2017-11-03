# PLDA - homework 1

For this homework you have to implement an interpreter for a turing complete language. For the most part you can consider it to be an extended lambda calculus.

I will be using an unconventional notation to describe the language. I will be using [EBNF grammar](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form#Example) to describe the abstract syntax tree (AST), rather than the grammar. This way it ought to be precise enough to describe the overall-form while not locking you into a specific implementation.

## Deadline

Midnight, Sunday, November 19th.

## The language AST:

We have two `terminal` elements which I will not describe any further as it should be obvious: `number`, `string`. `+` means that the term it is applied to occurs 1 or more times. `*` means it occurs zero or more times.

```bnf
<program> ::= <expr>

<expr> ::= <const>
        |  <val>
        |  <binary-operation>
        |  <eq>
        |  <if>
        |  <lambda>
        |  <apply>
        |  <val-decl>

<const> ::= integer

// variable dereferencing. i.e. "using" a variable. See the let expression for "declaring" a variable
<val> ::= string

// these are all binary arithmetic operators: + - * /
// semantics should be obvious.
<add>  ::= <expr> <expr>
<sub>  ::= <expr> <expr>
<mul>  ::= <expr> <expr>
<diff> ::= <expr> <expr>

//the comparison method. Semantics like in C, non-zero for equality, zero for inequality.
//It doesn't make any sense to compare function values
<eq>   ::= <expr> <expr>

//first <expr> represents the condition, zero is equivalent to "false", non-zero to "true"
// if "true", the "then" branch is represented by the second expression
// if "false", the "else" branch is represented by the third expression
<if> ::= <expr> <expr> <expr>

<binary-operation> ::= <add> | <sub> | <mul> | <diff>

// roughly equivalent to Haskell's "let" expression. Syntactically it would look something like:
// let x = 42 in x * 42
// and, you can define multiple variables at the same time:
// let x = 42; y = 1 + 2 in x * y * 10
//
// here, "string" is the variable name, and the immediately next <expr> represents its value
// the second <expr> is the "body" of the program where the defined variables ought to be available
<val-decl> ::= (string <expr>)+ <expr>

//defining an anonymous function with one or more parameters, and a function body
<lambda> ::= (string)* <expr>

// the first <expr> has to evaluate to a "function" value. Either directly through a use of <lambda>,
// or through dereferencing via <val>
<apply> ::= <expr> <expr>*
```

## What is to be done

Define this AST (roughly) as it is described in the above BNF grammar, and then write a function/method that takes as input a `<program>` in AST form, and gives you an result. The value of the program can either be an `integer` or a `function value`, however you chose to represent that.

## Semantics
* `<binary-operation>`
  * work only on integers values
  * fail at runtime if you try to apply them to any other data-types (e.g. function values)
* `<eq>`
  * roughly equivalent to the C equality operator. Returns "zero" when the two things you're comparing are not equal, "non-zero" if they are
  * should fail at runtime if you're comparing something else than numbers. We are, after all, defining a strongly typed dynamic language.
* `<if>`
  * we model booleans like in C, with integers. But the `if` is similar to the one in Haskell, where you are _required_ to define both branches, and it is an expression that always returns a value
* `<val-decl>`
  * if you define two variables in the same `<val-decl>` expression, the value of the first one has to be available for the definition of the second one. So for something like: `let x = 42; y = x * 10 in ...`;
  * recursive definitions should also be supported. If you define a variable with a `<lambda>`, that variable should be useable inside the definition of said lambda.
* `<val>`
  * semantically this boils down to "using" the value of a variable
  * fails at runtime if the variable that is being dereferenced is not defined anywhere
* `<apply>`
  * semantically this is "calling" a function. This is a technical term, hence the special status of the `apply` method in Scala (see labs for plenty of examples).

## Forms of freedom

Roughly speaking, you have to implement a language that has the ability to "define and use variables", to "define and use" anonymous functions, has branching ability, and supports recursion. Therefore, instead of modelling a `let-like` variable declaration scheme you can model it like in Scala with sequenced `val x = ...` declarations, and modify your AST accordingly.

## Extra work

I will detail this section a bit later. But throughout the semester you can implement things like a module system, a parser, and so one, which will grant the possibility to slack off for your 3rd homework. //TODO: add more detailed descriptions.

## Acceptance criteria

* do not use mutable state in your implementation. Therefore uses of `var`, and any collection in the `scala.collection.mutable` are banned. Homework will be heavily penalized for using mutable state. If you ever use mutable state, you will have to be prepared to defend your choice through the most relentless scrutiny.
* abide by the semantics described in the previous sections. You are allowed to expand the language — like introducing "boolean" values instead of using integers for conditionals, but you are not allowed to turn `if` from a Haskell-like `if`, into a C-like `if`.
* To get a passing grade, you have to support variable declaration (`<val-decl>`) and dereferencing (`<val>`); or `<apply>`. Without at least one of these features, your language and implementation are no more complex that the `JsonPrettyPrinter` we defined in the labs.
* programs that do not compile are not accepted — therefore make sure you can do a `sbt compile` in your terminal, for your project.
* a reasonable smoke test for all the features in the language is expressing the computation of the `nth` Fibonacci number in  your AST, and successfully running it through your interpreter. In most cases, this single test should be enough to convince me that all your features work. But writing code without tests for simpler cases is an invitation for you to fail implementing it, so write fewer tests at your own risk :)

## Stuff to read

* [labs](../labs) on everything Scala. The `JsonPrettyPrinter` serves as a basic starting point for everything non-variable, non-function related.
* basically google "implement a lambda calculus intepreter in `$YourLanguageOfChoice`" to get the basic gist of it in something you understand
* [`Essentials of Programming Languages`](https://karczmarczuk.users.greyc.fr/TEACH/Doc/EssProgLan.pdf) — Daniel P. Friedman and Mitchell Wand — is a great book that explains everything in excruciating details — implementations are in a LISP-like dialect
