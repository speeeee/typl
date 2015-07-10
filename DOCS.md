Documentation
====

# Syntax of LISP-style languages

As mentioned before, typl's syntax resembles the syntax of LISP.  This means that the syntax is usually very simple and consistent. One of the main features of LISP and its variants is homoiconicity.  Homoiconicity essentially means that its syntax is represented in the same data structure.

LISP's syntax consists of, as the acronym implies, lists.  These lists are special in that the head of the list is parsed as a function name while the tail of the list is parsed as
arguments for that function.  For example:

```
(+ 1 2)
```

Here, the head is `+`, and the tail is `'(1 2)` (`'` denotes a different kind of list that is not parsed as a function).

All LISP variants also allow using expressions within others:

```
(+ (+ 1 2) 3)
```

This expression simply means that it will add `(+ 1 2)` and `3`.  `(+ 1 2)` evaluates to `3` and then is added to the other `3` to make `6`.

There are plenty of resources on the Internet about LISP and its variants that go far more in-depth than what is explained here.  The rest of the documentation focuses specifically on typl.

# Syntax of typl

typl has a very similar syntax to LISP, though with one major difference.  The top-level expression is not closed with parentheses, rather instead it has an `!` at the end of the expression, similarly to how a semicolon would work in C-variants.

```
+ 1 2!
```

Expressions within the top-level expression look exactly the same as they would in LISP:

```
+ (+ 1 2) 2!
```

The `!` carries no significance other than to save a character.  The compiler simply changes it to a LISP-style expression in the end.  However, this does not mean that a LISP expression can replace it.  The `!` is an initiative for the compiler to process the expression.  If it is not there, then the list is not parsed, and the list itself is returned.

Despite the similarities to LISP, typl is not necessarily a LISP dialect.  The syntax of the language is where the similarities end.

# Types of expressions

As expressed before, the head of every list is processed as a function to be applied to the arguments, which are the tail of the list.  However, there are two types of expressions that typl makes use of: functions and predicates.  Both functions and predicates make use of the concept of gates.

A predicate is any expression that returns a boolean.  In this sense, predicates can be interpreted as a subset of functions.  However, there some more differences to cover.

# Predicates in typl

Below is a definition of a predicate in typl:

```
pred positive (list (> a 0))!
```

At first, the last argument of the definition might seem odd, but the previous should be covered first.  `pred`, as mentioned before, is the function call.  `positive` is what this new predicate should be named.  It should be inferred from the name that the predicate takes a number (an integer to keep things simple) and determines if it is above 0.

The second argument seems somewhat odd at first though.  This is what is called a 'gate'.  A gate is a list of predicates that must be satisfied in order to pass.  Here, if the input is greater than 0, the gate is passed and `positive` returns true.  If it helps, the gate can be thought as a list of `and` statements; all must be satisfied in order to return true.  This introduces another part of predicates. **All expressions within a predicate must also be predicates.**  Because of this, a gate can almost be seen as an anonymous predicate.  This is why functions like `print` actually return a boolean in typl.

Another thing to note is that the type of the variable `a` was never specified.  This is because the predicate, `>`, already takes 2 integers as arguments.  Because of this, the compiler can infer that `a` is an integer.

# Functions in typl

Unlike predicates, functions can return anything.

Here is a funciton definition:

```
: square (list (Int a)) (* a a)!
```

`:` is the same as `define`, it starts a function definition.  `square` is the name of the function being defined, and what is to be accomplished is that the input be squared.  

The gate is also a part of functions as well.  It evaluates the predicates just like it would in a predicate definition.  The fundamental difference, however, is that instead of returning a boolean, it will produce an **error**, either at compile-time or at run-time, if the gate isn't passed.  Whether the error appears at run-time or compile-time is explained in the next section.  The gate here essentially confirms if `a` is an integer.

The third part is the actual definition.  This is run if the gate is passed.  Here, the definition is simply the multiplication of `a` and itself, which is how squaring is done.

With this function defined, it can now be used:

```
(square 2) ; returns 4
(square "2") ; returns compile-time error
```

# When a gate of a function is not passed

As mentioned before, an unpassed gate of a function returns an error.  The ambiguous part, however, was when.  When it returns an error depends on the kind of predicate that is not passed.  It sounds convoluted, but it actually is simple.

Take the previous `square` definition.  Notice the single predicate, `(Int a)`.  Now notice that the example, `(square "2")`, returns an error at compile-time.  The reason for this is that `Int` is a special kind of predicate.  As mentioned before, this language compiles to C.  The reason why `Int` returns a compile-time error is because it has a direct relation to the C-primitive, `int`.  Because of this, the compiler can easily catch a type mismatch.  However, a predicate like, `(> 1 0)`, has no C-type that it relates to.   Because of this, `(> 1 0)`, is instead evaluated at run-time.

Also, errors related to literal types like `List` (produced by using the `list` function) are also caught at compile-time. 


