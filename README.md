ECS - Entity Component System - is a paradigm used for writing game code.

This is a VERY simplistic implementation of ECS in CL and CLOS, specifically for SBCL (`sb-mop:` is not meaningful in LispWorks, but is required in SBCL).

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
Contains the (very few) Components (aka "attributes") that an Entity can include.  I define only 2 kinds of things
1. `ECS-Position`, mutable and immutable, called "Movable" and "Stationary" respectively.
2. `Facial-Expression`.

This file, also, defines how to display components (`defmethod print-object`).

## entities.lisp
Contains templates for 2 kinds of game Entities
1. `Playing-Character` (the player is defined to be one of these)
2. `Stationary-Item` (the rock is defined to be one of these)

This file, also, defines how to display Entities (`defmethod print-object`).

## systems.lisp
Creates 2 kinds of transmogrifiers
1. `system-increment-position`, and,
2. `system-change-facial-expression`.

N.B. you don't need to prefix the names of these functions with `system-`.  This is just my personal preference, for clarity (hopefully).
## ecs.lisp
Kicks things off in the function `(main)`. 

It 
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
says: if `p` is of type `Movable` and `dx` is of any type and `dy` is of any type, then the body of code can be applied to it.  The arguments `dx` and `dy` are not qualified by type, hence, they are assumed to be ultra-generic (of type `T`).

There can be more than one definition for each method (qualified function).  For example ```
```
(defmethod proc-increment ((p T) dx dy)...
```
fires if `p` doesn't match other possibilities.

# Ugly Syntax
Lisp is, actually, an assembler, with "recursive" syntax, instead of linear syntax.  As such, Lisp is a wall of functionality. It is a Good-Thing to have a good memory, or an always-open tab on [CLHS](https://www.lispworks.com/documentation/HyperSpec/Front/index.htm)

CLOS is a class-like type system implemented in Lisp (Common Lisp to be exact).  CLOS is better than most class-based type systems. There are lots of niggly little details that are needed to create a class-like type system. CLOS chooses to make each such niggly detail explicit, but, leaves the details on the table, so you can play with them and use introspection at will (but, of course, this approach leaves you drowning in detail :-).

`... (pos :type 'Movable :accessor pos :initarg :pos)...`

This line says 4 things:
1. There is a field (aka "slot") named `pos`
2. The field is defined to be of CLOS type `Movable` (defined elsewhere, the definition must be loaded before the compiler hits this line of code).
3. The `getter` and the `setter` functions are called `pos` (":accessor")[^setf]
4. To create a literal of this type, use the `make-instance` function and supply field initializers called `:initarg`s. For this field, the `:initarg` is `:pos` (exactly those 4 characters `:`, `p`, `o`, `s`). Initargs have to come in pairs - the name, followed by a value. This kind of thing is akin to *named parameters* in more modern languages.

[^setf]: accessors can be used in SETF to become setters

In CLOS, you specify a method call by stating the method name first (the accessor), then the object's name, all in parentheses, e.g. `(pos player)`.

You can get even more specific, if you want. Feel free to browse the legalese in the Common Lisp HyperSpec (CLHS) for more detail.


# Offensive Wording
I feel that the name `ECS` is wrong.

Instead of Entity / Component / System, it might be something like Entity / Attribute / Transmogrifier.

`EAT`.


