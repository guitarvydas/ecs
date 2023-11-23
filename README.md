ECS - Entity Component System - is a paradigm used for writing game code.

The project herein is a VERY simplistic implementation of ECS in CL and CLOS, specifically for SBCL[^sb-mop].

[^sb-mop]: `sb-mop:` is not meaningful in LispWorks, but is required in SBCL.

My imagination is stunted.  In this example, I build 2 do-nothing Entities: player and rock.

The player has two attributes ("Components"):
1. position (mutable)
2. facial-expression (mutable).

The rock has only one attribute:
1. position (immutable).

The function `(main)` (in `ecs.lisp`) initializes the 2 Entities, prints out their details, then steps the ECS engine once, by changing the player's position by (1,1) and the facial expression.

The engine, also, tries to step the "rock", but nothing happens (as it should).

# Usage
`chmod a+x run.bash`
`./run.bash`

(note: you need to have SBCL installed)

# Excrutiating Details
## components.lisp
Contains the Components (aka "attributes") that an Entity can include.  I define only 2 kinds of things
1. `ECS-Position`, mutable and immutable, called "Movable" and "Stationary" respectively.
2. `Facial-Expression`.

This file, also, defines how to display components (`defmethod print-object`).

## entities.lisp
Contains templates for 2 kinds of game Entities
1. `Playing-Character` (the *player* is defined to be one of these)
2. `Stationary-Item` (the *rock* is defined to be one of these)

This file, also, defines how to display Entities (`defmethod print-object`).

## systems.lisp
Creates 2 kinds of transmogrifiers
1. `system-increment-position`, and,
2. `system-change-facial-expression`.

### Defaults
The way that I chose to write this code, makes it OK to have no default for `system-change-facial-expression`, but, makes it necessary to provide a default for `system-increment-position`.  

For some - arbitrary - reason, I chose to include *position* attributes in both Entities in this example, where *Movable* and *Stationary* are sub-classes of *Position*.  

The code, as written, checks to see if a slot exists, and, if so, calls a method on the slot.   I included *Facial-Expressions* in `Playing-Character` Entities, but, not in `Stationary-Item` Entities, so the existence-check skips over `Stationary-Item`s when trying to change facial expressions.  On the other hand, I included `pos` fields in, both, `Playing-Character` and `Stationary-Item` Entities, which means that the existence check passes and the system tries to call a method on each of these attributes.  For the *rock*, we want to ensure that the method call falls-through to the default (do nothing), so I had to write a default `defmethod` for such a case.  You can choose to do this in whatever way pleases you when writing your game.  

If I wanted to be VERY explicit, I would have included default code for every `system-` method, to show other (human) readers that I really meant to do nothing.  In fact, if I had included default code for every `system-` method, the existence-check would not ever be needed (`(when (slot-exists-p e 'face) ...`).  Keeping the existence-check, though, allows more flexibility during the early stages of the game design, and, follows more closely the ECS paradigm, and reduces noise in the code base.

N.B. you don't need to prefix the names of these functions with `system-`.  This is just my personal preference, for clarity (hopefully).
## ecs.lisp
Kicks things off in the function `(main)`.  It 
1. creates a `player` and a `rock`
2. prints out the initial state of the game
3. then steps the game once, and,
4. prints out the result.

## CLOS Method Lookup

CLOS uses a Type system to fire methods on objects. (CASE-on-Type - very OOP-ish).

You can see this expressed in `systems.lisp`.

Each function is declared with the keyword `defmethod` instead of with the keyword `defun`.

The arguments to `defmethod` can be qualified by type, e.g. 
```
(defmethod proc-increment ((p Movable) dx dy) ...
```
says: if `p` is of type `Movable` *and* `dx` is of any type *and* `dy` is of any type, then the body of code can be applied to it.  The arguments `dx` and `dy` are not qualified by type, hence, they are assumed to be ultra-generic (of type `T`).

There can be more than one definition for each method (qualified function).  For example ```
```
(defmethod proc-increment ((p T) dx dy)...
```
fires if `p` doesn't match other possibilities[^qt].

Note that CLOS allows you to qualify any and all parameters to `defmethod`.  This allows you to put a finer grain on how lumps of code get fired.  In this - simple - example, though, I only qualify one parameter.

[^qt:] Did I need to qualify `(p T)` or was I just being pedantic?  My eyeballs like to pick up on the pattern - `p` is qualified as `Movable` in one case and qualified as ultra-generic `T` in another case.
# Ugly Syntax
Lisp is, actually, an assembler, with "recursive" syntax, instead of linear syntax.  As such, Lisp is a wall of functionality. It is a Good-Thing to have a good memory, or an always-open browser tab on [CLHS](https://www.lispworks.com/documentation/HyperSpec/Front/index.htm)

CLOS is a class-like type system implemented in Lisp (Common Lisp to be exact).  CLOS is better than most class-based type systems. There are lots of niggly little details that need to be handled to create a class-like type system. CLOS chooses to make each such niggly detail explicit, but, leaves the details lying on the table, so you can play with them and use introspection at will (but, of course, this approach leaves you drowning in detail :-).

`... (pos :type 'Movable :accessor pos :initarg :pos)...`

This line says 4 things:
1. There is a field (aka "slot") named `pos`.
2. The field is defined to be of CLOS type `Movable` (defined elsewhere, the definition must be loaded before the compiler hits this line of code).
3. The `getter` and the `setter` functions are called `pos` (":accessor")[^setf].
4. To create a literal of this type, use the `make-instance` function and supply field initializers called `:initarg`. For this field, the `:initarg` is `:pos` (exactly those 4 characters `:`, `p`, `o`, `s`). Initargs have to come in pairs - the name, followed by a value. This kind of thing is akin to *named parameters* in more modern languages.

[^setf]: accessors can be used in SETF to become setters

In CLOS, you specify a method call by stating the method name first (the accessor), then the object's name, all in parentheses, e.g. `(pos player)`, which might be written as `player.pos()` in other languages (like JavaScript).

You can get even more specific, if you want. Feel free to browse the legalese in the Common Lisp HyperSpec (CLHS) for more detail.

# Appendix - Common Lisp Implementations
There are many implementations of CL.

My favourite, currently, is Lispworks.

My least favourite, currently, is SBCL.  It is very pedantic and extremely correct.  I find that this pedanticness interrupts me during the Design process.  My typical workflow is to develop in Lispworks, then, sometimes, to bump the result into SBCL to keep me honest.  Most people seem to use SBCL and Emacs and Slime and ASDF and Quicklisp as their workflow.

There are implementations of CL on JVM - Armed Bear is one.

There are implementations of CL meant to be used as embedded languages - ECL is one.

Quicklisp is a (free) package manager for CL that makes workflow especially easy.

ASDF is a system definition DSL for CL, kinda like `make`, but, geared towards CL.  Quicklisp strongly encourages the use of ASDF in the workflow.

Common Lisp is a union of all of the Lisp variants that existed in the early 1990s. Scheme  is at the other end of the scale - it is a culled version of Lisp meant for research.  Racket is a modernized version of Scheme and includes a huge (and disjoint and confusing) library of functionality.  I chose CL over Scheme, because I wanted to build commercial apps and needed everything including the kitchen sink.  Culled Lisp was beautiful, and understandable, but impractical.  Maybe Racket would be more practical, but, wasn't available to me when I made the choice.

Scheme, and, therefore, Racket emphasize static typing.  I find that static typing interrupts my workflow during Design stages.  I feel that static typing is wonderful if you specifically want to Production Engineer an already-working product.  I want a dynamically-typed language when I'm Designing a solution, not a statically-typed language.  Static typing is for later on in the workflow.

CL *does* allow gradual typing.  Once you have working code, you can add more type details to the code and supposedly get more efficient code as a result.

The original intent of CL was to compete with the very-hot FORTRAN compilers of the day.  CL *can* be *interpreted*, especially when you are just goofing around and in Design mode and want a rich debugging environment, but, CL is usually *compiled* to machine code.

CL is termed to be a Lisp2, whereas Scheme is a Lisp1.  In a Lisp2, an identifier (a "symbol") can have many meanings and the compiler decides on what meaning to use depending on context.  For example a symbol can contain a value *and* it can be a *function* at the same time - the syntax rules specify which meaning gets used and the compiler uses the rules to understand what you mean. In a Lisp1, like Scheme, you get to use a symbol for one kind of thing at a time.  The difference is subtle, for example in CL you can use the name "list" as a builtin function name, and, you can use it as a variable name in the same code.  In CL, though, you have to use `function` when you want to call a function that has been passed in as a parameter.  In Scheme, you can assign a *function object* into a symbol and just use it as a function without the extra `function` keyword.  There are good arguments for using Lisp2 and for using Lisp1 - the choice isn't clear.  I make the decision to use either one depending on other factors (like existence of library functions that I want to use) and then just go with whatever choice - Lisp2 or Lisp1 - that I am forced to use.

I love Lisp because of its assembler-like simplicity.  To me, Lisp is assembler, but with a recursive syntax instead of a linear syntax.  Lisp is just a bag of functionality with minimal syntax (AST syntax - Lists are Trees).  True Lisp - being a bag of functionality - allows one to craft programs in *any* paradigm. From this perspective, Clojure is not Lisp, it's just Functional Programming with a miserable syntax. Just a single paradigm.  Common Lisp is a mature, industrial form of Lisp (the kind of assembler-y Lisp that I like).  Scheme is an academic, stripped-down, impractical version of Lisp leaning heavily towards static compilation (anti-Lisp, if you ask me).  Racket is a more practical version of Scheme.  Racket has re-discovered PEG parsing and macros and, hence, declares itself to be a "language workbench".  Others would rate Rebol as the more-true "language workbench".  I've discovered OhmJS and no longer wish to use an all-in-one "language workbench" or Rebol.

Clojure was developed by a Lisper.

Macros are an invention of Lisp.  Macros  look like functions.  When you look at Lisp source code containing functions and macros, you can't tell which is which, without deeper knowledge. Theoretically, it should be possible to use macros to define little DSLs that help you express a problem and its solution.

CL macros are "non-hygienic", which means that symbols inside a macro can refer to symbol outside of the macro (textual leakage).  This is particularly flexible, but, comes with a gotcha in that sometimes, macro symbols clash with other symbols in the source code.  To fix this problem, Scheme-ists developed "hygienic macros" that scope variables and make name-clashes impossible.  The costs of creating and using hygienic macros consist of added complication. In my mind, the added complication of using hygienic macros is so high that it interferes with workflow during Design and, essentially, destroys the "creative" Design process.

If we step back an look at the hygiene problem, we see that the problem is actually Accidental Complexity caused by forcing *transpilation* into a one-size-fits-all language-and-compiler.  If one were to preprocess macro code, then feed the preprocessed code into a compiler, the problem of hygiene goes away - a "DSL" for separate preprocessing is hygienic by definition.  This approach used to be considered difficult and inefficient because it used to be difficult to build DSLs and preprocessors, and, because preprocessing would take up too much turn-around time.  Today, though, PEG technologies, like OhmJS make it easy - and quick - to build new syntaxes and new DSLs and little transpilers. Only the turn-around problem remains. OhmJS can be run more than once in the same program (currently JavaScript, for OhmJS - other PEG approaches exist). PEG uses backtracking, so the recursive expansion of macros is simplified.  We hang syntax-coloring daemons onto programming editors today, why not PEG preprocessors?  

A new problem occurs with the preprocessing approach - preprocessed code doesn't automatically integrate with the original source code.  This problem was attacked by C's `#file / #line` directives.  These directives appear in much more modern programming languages like Odin.  It should be possible to tie source code to its expanded version.

Lisp macro have a limitation.  Lisp macros work only on source code that is represented as *lists*. Again, PEG / OhmJS work at a lower level, with characters, and, hence, should be usable for writing and expanding macros in just about any textual language (and text documents not intended for use as program source code).

GC - Garbage Collection - was invented by Lisp.  This is a feature that lubricates Design Flow - programmers don't need to worry about niggly details like memory allocation and freeing.  Again, when one *needs* to optimize a software product, one should go into a Production Engineering workflow and profile and preen code, removing normalizations like GC in places where it matters.  Starting out with that mindset, though, is anathema to Flow during Design.

The most beautiful Garbage Collector that I've seen is that in Sector Lisp. It is only 40 bytes[sic] long.  It is so small, not because it is written in assembler using assembler tricks, but because it is truly functional. There is no concept of a heap - no RAM, no mutation - in Sector Lisp.

# Appendix - Offensive Wording
I feel that the name `ECS` is wrong.

Instead of Entity / Component / System, it might be something like Entity / Attribute / Transmogrifier.

`EAT`.


