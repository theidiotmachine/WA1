
# White paper: a design document for an untitled WebAssembly programing language <!-- omit in toc -->

- [Introduction](#introduction)
- [Defining philosophical principles](#defining-philosophical-principles)
  - [1 - Request-response](#1---request-response)
  - [2 - Development speed and runtime speed](#2---development-speed-and-runtime-speed)
  - [3 - Pragmatism with a sound theoretical basis (aka 'cunning and brutal')](#3---pragmatism-with-a-sound-theoretical-basis-aka-cunning-and-brutal)
  - [4 - The path of least resistance should be to the safest code](#4---the-path-of-least-resistance-should-be-to-the-safest-code)
  - [5 - Learn from other places, postmodernism](#5---learn-from-other-places-postmodernism)
- [Underpinning technical principles](#underpinning-technical-principles)
  - [1 - Pass-by-value](#1---pass-by-value)
  - [2 - Garbage collection](#2---garbage-collection)
  - [3 - Strong and deep immutability](#3---strong-and-deep-immutability)
  - [4 - Expression-based language](#4---expression-based-language)
- [Features](#features)
  - [Mundane syntax choices](#mundane-syntax-choices)
    - [Indenting](#indenting)
    - [Colons](#colons)
    - [Semicolons](#semicolons)
    - [Casing](#casing)
  - [By default, symbols are constants, not variables](#by-default-symbols-are-constants-not-variables)
  - [Block rules](#block-rules)
  - [Low-level types](#low-level-types)
  - [Other simple types](#other-simple-types)
  - [Type inference](#type-inference)
  - [`if` statements](#if-statements)
  - [Simple static functions](#simple-static-functions)
  - [Generic functions](#generic-functions)
  - [Traits](#traits)
  - [Unsafe mode](#unsafe-mode)
  - [Type guards](#type-guards)
  - [Ints](#ints)
  - [The standard object model](#the-standard-object-model)
    - [Allocation types](#allocation-types)
      - [Stack](#stack)
      - [Heap by-value](#heap-by-value)
      - [Heap by-ref](#heap-by-ref)
    - [Type qualifiers](#type-qualifiers)
      - [Const](#const)
      - [Mutable](#mutable)
      - [Borrowed reference to heap value mutable](#borrowed-reference-to-heap-value-mutable)
  - [Class declarations](#class-declarations)
  - [Generic classes](#generic-classes)
  - [Member functions](#member-functions)
  - [Static member functions](#static-member-functions)
  - [Alternative constructor syntaxes](#alternative-constructor-syntaxes)
    - [Scala-like](#scala-like)
    - [Rust-like](#rust-like)
  - [Traits again](#traits-again)
  - [Inheritance](#inheritance)
  - [Arrays](#arrays)
  - [Iterators](#iterators)
  - [By-ref arrays](#by-ref-arrays)
  - [Linking](#linking)
    - [An entry point](#an-entry-point)
    - [Another keyword](#another-keyword)
  - [Strings](#strings)
  - [Tuples](#tuples)
  - [Lambdas](#lambdas)
    - [Basic implementation](#basic-implementation)
    - [Type](#type)
  - [Enums, `switch`, Options](#enums-switch-options)
    - [Enums](#enums)
    - [`switch`](#switch)
    - [Back to enums](#back-to-enums)
  - [Modules](#modules)
  - [I can't believe you read this](#i-cant-believe-you-read-this)
- [Research notes](#research-notes)
  - [Trait and interface default implementations](#trait-and-interface-default-implementations)
  - [Trait and interface data members](#trait-and-interface-data-members)
  - [Trait functions with extra type conditions](#trait-functions-with-extra-type-conditions)
  - [Reflection](#reflection)
  - [Operators](#operators)
  - [Function purity](#function-purity)
  - [Tail recursion](#tail-recursion)
  - [Type guards and mutability](#type-guards-and-mutability)
  - [Commas can trail, commas are optional](#commas-can-trail-commas-are-optional)
  - [Type of a constant object with the default constructor](#type-of-a-constant-object-with-the-default-constructor)
  - [Mocking at compile time](#mocking-at-compile-time)
  - [Interfaces are a pair of pointers](#interfaces-are-a-pair-of-pointers)
  - [Immutable arrays don't need capacity](#immutable-arrays-dont-need-capacity)
  - [Tuples as function arguments; Void as the zero tuple](#tuples-as-function-arguments-void-as-the-zero-tuple)
  - [Lambdas that mutate variables](#lambdas-that-mutate-variables)
  - [Holding `heap ptr` classes in other classes](#holding-heap-ptr-classes-in-other-classes)
  - [Promotion of functions to typeguards](#promotion-of-functions-to-typeguards)
  - [Being honest about our type system and proofs](#being-honest-about-our-type-system-and-proofs)
  - [Run-time type information](#run-time-type-information)
  - [Boxing pushed down to the compiler](#boxing-pushed-down-to-the-compiler)
  - [Unions](#unions)
  - [Packing in data - option sizes](#packing-in-data---option-sizes)
  - [Inlining](#inlining)
  - [Laziness](#laziness)
  - [Matching](#matching)
  - [Docs](#docs)
- [Things that WASM needs](#things-that-wasm-needs)
  - [A garbage collector](#a-garbage-collector)
  - [A good string library](#a-good-string-library)
  - [A good way of shipping runtime](#a-good-way-of-shipping-runtime)
  - [Access to TZ](#access-to-tz)
## Introduction 

This document defines the MVP for a proposed WebAssembly language. The WA1 prototype that it comes with implements some of this, and fails to implement others, but generally acts as a research project.

This document is a white paper, because that sounds much better than 'random bag of terrible ideas'. We use the first person plural because Science! And it's a habit we got into when we were Queen Victoria. Also it means if we ever have a friend it will be easy to make it look like this was a group effort!

We don't know what to call the language, by the way. WA1 is a terrible name, but it is better than all the others. We pronounce it 'Wah-won'.

## Defining philosophical principles 

### 1 - Request-response

This language is intended as what we are calling a _request-response_ language. This is because it is aiming for the niches that JavaScript currently occupies. These are:
1. in a browser: responding to, e.g. user inputs, and
1. in a web server: responding to requests to, e.g. return assets.

We also imagine it being useful where Lua (and to a degree JavaScript) is used: as a scripting language within a larger system, e.g. a game or database.

In practice this means that the language will be mostly used to consume data, transform it, and pass it on to somewhere else. 

### 2 - Development speed and runtime speed

This language is competing against scripting languages. A scripting language abandons some aspects of safety and speed for developer experience. It does this through e.g. loose typing, various flavours of JIT compilers and high level abstractions.

JavaScript is an extreme example of this. TypeScript rows back on some of these with a stricter type system and an AOT compile step, meaning there is appetite in the community for a tighter language than JavaScript. 

However, simply writing a JavaScript/TypeScript style language is not useful - AssemblyScript is already aiming at this goal.

The intent of the language is that, compared to Rust, the language should favour development speed; but compared to JavaScript the language should favour runtime speed and safety. We may perhaps be able to link with a faster language such as Rust or C if we need extra speed in critical sections.

### 3 - Pragmatism with a sound theoretical basis (aka 'cunning and brutal')

There are many beautiful and un-adopted languages in the history of Computer Science. Languages with a rigid set of principles are often only useful in certain circumstances, or have a niche appeal. 

However, there are a lot of modern ideas that unprincipled languages cannot adopt. Correctness proof, for example, is something that only a subset of languages can achieve.

So we should adopt the best of computer science, but tempered with pragmatism. This means that when a sound theoretical principle is useful it can be used, but an escape hatch is available if needed. This is obviously similar to Rust's 'unsafe' mode; but the other motivating example is Scala's ability to drop down to `while` loops where speed is necessary.

### 4 - The path of least resistance should be to the safest code

Even though the language provides escape hatches, the simplest thing to type should always be the safest. 

This is motivated by the fact that, even now, we see JavaScript examples that use `var` rather than `let`. We think this is because there are no advantages from a syntax point of view, and so developer's fingers are hardcoded to type the less-safe `var`. We wish to head this off now, while we are designing the language.

### 5 - Learn from other places, postmodernism

JavaScript was not novel. Even its name was 'borrowed' from the Java programming language. We have no delusions of being grand language creators, and want to learn from the best bits of languages we have used.

Given our borrowings will be fairly broad, this means we will end up with a postmodern language, which is a box of useful tools; rather than a modernist language which is rigidly built around a central set of ideas.

## Underpinning technical principles

Unlike the previous philosophical principles, these are technical principles that the language features are built on.

### 1 - Pass-by-value

This language is pass-by-value by default.

Pass-by-value is conceptually thread-safe (although implementations may not be) and so is useful for a language that is designed for request-response. Small objects can be passed in their entirety on the stack, which tends to be cheaper than heap allocation. Storage of objects can be inlined, meaning less overhead.

Of course, pass-by-value of large objects is considerably more expensive than pass-by-reference. We discuss ways to mitigate this later, but this is still a potential problem.

### 2 - Garbage collection

This language uses a garbage collector.

There are two reasons for this. Firstly, the WebAssembly spec contains a proposal for a garbage collector. Using this means we can lower the amount of runtime we will have to ship with each program, reducing the size of shipped binaries. This is a critical part of speed on the web.

The second is that the request-response nature of the language should provide natural pauses where garbage collection is appropriate, and somewhat mitigate the concerns of users about garbage collection stopping execution during useful work.

Interestingly, the pass-by-value model reduces the need for a garbage collector somewhat. This is discussed later.

### 3 - Strong and deep immutability

Here we use 'strong' immutability to mean that a user cannot trivially override immutability, such that we assume our compiler can optimise around it. There is no `const_cast` or similar.

'Deep' immutability means that if an object is immutable, every object it contains is also immutable. This is similar to Rust's immutability model.

### 4 - Expression-based language

The language is expression-based, meaning that every construction returns a value. Sometimes that value is a special value (`Never` or `Void` or `Unknown`) but it still has a type in the type system.

## Features

From simple to complicated, these are the proposed features of the language.

### Mundane syntax choices

These syntax choices, depressingly, are probably the most important part of the language success.

#### Indenting

There are broadly three styles of language syntax: whitespace based (Haskell, Python), keyword based (Pascal, Lua), and punctuation based (C, Lisp).

We chose C style because that has been the most successful of the three. As an aside we also think that Haskell and Python programmers will be so horrified by this language (it has loops and variables! It has types and encapsulation!) that there is no point aimimg for their custom. 

So, blocks are declared with curly brackets (`{}`).

#### Colons

We did, however, make a concession to modern languages by using colons for type declaration. This has been accepted in Rust, Scala and Typescript. We think it is easier to read and is nice for implicit type declaration. This means that a symbol `s` with type `T` is declared like this.

```
s: T
```

#### Semicolons

Semicolons are optional. A return character is equivalent to a semicolon, like Scala. TypeScript shies away from this because there are frightening ambiguities, and linters enforce semi-colons.

```typescript
//this code returns undefined
return
    4
```

Scala, however, manages to succeed here, and we think we can too. (Scala actually does have a single ambiguity around anonymous derived classes: it works around this by enforcing K&R (or 'Egyptian') style indenting. We use K&R because we like it. Anecdotally, the Scala lexer is unpleasant because of semicolon ellison. We will cross that bridge when we come to it.)

#### Casing

Types are largely UpperCamelCase, including built-in types. So, `Bool`, `Option` and so on. Scala does this, and it is actually rather nice. The only exception to this are some types derived from constant values. We don't think there is much value in differentiating between user and built-in types.

Variables, functions, and other non-type things are lowerCamelCase.

### By default, symbols are constants, not variables

In TypeScript the text to declare a variable (`let`) is shorter than for constant (`const`). We think this is poor UX.

So, the following syntax is used for constants and variables.

```
//This is a number that can't be changed
let a: Number = 3

//This is a number that can be changed
let var b: Number = 4
b = 6
```

Note that `let var` is used to declare a variable that can hold more than one thing over the course of its life time. It is *not* used to declare a variable that can mutate the thing is is holding. There is obviously no distinction with numbers, so this is easier to understand with an example.

```
let var c: Whatsit = new Whatsit(1)
c = new Whatsit(2) //this is fine, c is pointing to something else, it is a variable
c.changeTheThing(4) //error! You may not mutate what c points to
```

### Block rules

A block is an expression that is composed of expressions. The return value of a block is the result of the last expression of the block. So for example

```
let a: Number = {
    let b: Number = 2
    b * 2
}
```

Here `a` is initialized to 4.

Because blocks are compound expressions, generally you can omit the curly braces when you have a single expression.

### Low-level types

`Unknown` is the top type. We have a conceptual hole here because you can't currently instantiate a thing of type `Unknown`, and you can't downcast from there. The name is borrowed from TypeScript. 

`Never` is the bottom type. This is obviously the type of an expression that never returns a value (such as a `return` keyword or a `__trap()`).

`Void` is a special unit type. This is the same as `()` in Rust and Scala, in that it pretends to be a type but is actually the 'no return value' symbol. Because it is a unit type, this code is legitimate.

```
let v: Void = Void //should be removed by the compiler
```

The implications of all this, is that this the following code is allowed, and we are not completely sure how to implement it. Scala manages (`AnyVal` is the base class of all value types including `Unit`) so it must be possible.

```
let u: Unknown = Void
```

### Other simple types

`Bool` is the boolean type. It has the two values `true` and `false`. We called it `Bool` rather than `Boolean` because why would we do that to our poor users?

`Number` is a 64bit float.

Constants of these types are also types. So, `false` is the type of the value `false`.

### Type inference

A constant with no type is considered to be the type of the constant it is initialized to.

```
let b = true //of type 'true'
```

A variable with no type is widened to a practical type.

```
let var b2 = true //of type Bool
```

### `if` statements

An `if` statement with an `else` branch is an expression. One without is not. (One without could be of type `Void` - up for debate.) The type of the `if` statement is very simple. If the two branches return the same type, it is of that type. If one can be coerced to the other trivially, then it is the coerced type. Otherwise it is a syntax error.

This is done because type inference in some languages (Scala in particular) is one of the main pain points in compiler time, and it seems reasonable to push a little cognitive load onto the developer in return for a faster compiler.

### Simple static functions

Simple static functions are declared using the `fn` keyword. They require a list of typed arguments and use Rust's thin arrow (`->`) to declare their return type. Because the last element of a block is its return and because a block of a single element does not need curly brackets, it is legal to have a one expression function without curly brackets.

```
fn double(x: Number) -> Number x * 2
```

Return type inference uses TypeScript's/Scala's fat arrow (`=>`). 

```
fn quadruple(x: Number) => x * 4
```

We have no type inference for the argument types. Again, with the Scala type inference compile-time cost in mind, we will probably use something like the TypeScript algorithm which is very simple and limited to lambdas. In addition, we dislike API functions that are not explicitly typed, and took the opportunity to require thin arrow explicit typing for `export`ed functions.

`return` is still a valid keyword, and there is nothing syntactically wrong with this.

```
fn square(x: Number) -> Number return x * x
```

However, if this was compiled naively (as a `return` instruction) some WebAssembly tooling sees this as an error, and we actually have to translate it into the simpler syntax.

The same type inference rules for `if` are used for `return`: that is, if we can naively coerce all expressions returned, then we use that type, otherwise it is a syntax error.

### Generic functions

This language has very limited support for 'true' generics - that is, a shared function that has its generic arguments erased to some low-level base type, with compile-time coercion applied where necessary. Instead, it uses what C++ calls templates: unique functions generated as needed. (The actual term is 'monomorphization'.) We hope that a post-link optimise step can aggregate and share identical functions. (Such a thing exists in Clang, but, we think, not in the LLVM WebAssembly toolchain.) 

There are a number of reasons for this. The first is that very simple types are not boxed; at the runtime we cannot interchange a `Number` with a pointer. Java gets round this with `double` and `Double` and we hate that. If we want to put small types on the stack with the multi-value extension, this is also hard. (Scala takes a different approach: it generates boxes automatically. This has a code-size vs speed trade-off. Perhaps we will do this later, but our bet is for the type of systems we are targeting there will be relatively few user-written generics.)

The second is that the standard memory model sometimes requires deep copies. This means that any assignment can potentially generate diverging code. This is one of the prices of a pass-by-value language.

A generic declaration uses what are optimistically called angle brackets, but are of course just 'less than' (`<`) and 'greater than' (`>`). This can introduce weird ambiguities, but everyone seems ok with this. We much prefer the Scala approach of square brackets, but we acknowledge that it never really caught on.

Here is the identity function.

```
export fn id<T>(x: T) -> T x
```

To call a generic function, one can always call it explicitly.

```
let a: Number = 3
let b = id<Number>(a)
```

However, there is some simple inference, using argument types. We can't currently do inference using return types.

```
let a: Number = 3
let b = id(a)
```

### Traits

Traits are inspired by (but are not as powerful as) Haskell type classes. Unlike in Rust, they are not generic interface types. In fact, a trait is not a type at all: it is a contract that a type can conform to. Here is a trait.

```
trait TraitOne{
    fn a() -> Void
    fn b(x: Int) -> Int
    fn c() -> This
}
```

You may notice the `This`. That's the type that conforms to this trait.

Because a trait is not a type, it cannot be used as an argument to a function. Instead, you must make a generic function and express that a type argument conforms to a trait. Then you can call functions from that trait.

```
fn callA<T: TraitOne>(x: T) -> Void x.a()
```

Generic arguments may be unions or intersections of traits. This uses the `|` and `&` operators.

```
fn oneOfThese<T: TraitTwo | TraitThree>(x: T) -> Int x.doSomething(5)
```

Unions of traits have the intersection of their functions. Intersections of traits have the union of their functions. Yes, this is TypeScript type union/intersection, but at compile time.

Traits can be built-in for useful features. Yes, this is stolen from Rust.

```
fn add<T: Numeric>(l: T, r: T) -> T { 
    l + r 
}
```

Why would you want this? This lets us push some code to compile time. Once you link, everything is just static functions, so you can figure out what functions are not called (something that vtables obfuscate) you can do whole program dead code elimination. It also lets us make our generics nice and powerful.

The downside is, of course, proliferation of code. A generic that uses a trait is pretty much guaranteed to spit out a unique function for every type.

### Unsafe mode

So far everything we have talked about is rooted in pretty solid theory. But we need a layer below all this, where the wires poke out, if we are going to write any runtime code, like an allocator or the internals of our array type.

Unsafe mode is that mode. We are not a low-level language like Rust; we don't have a few simple extra constructs. Essentially we need a C-like language where we can access raw memory. It's not very important to go through this, but we have an `__Ptr`, an `__Array` and an `__Struct`. (We pronounce `__` as 'unsafe'.) We also have some unsafe intrinsics, such as `__trap()`. 

We hope that, in the future, we won't need most of this because the work of the runtime will be done by WASM itself. We will note that we ported Doug Lee's malloc to this and it was amazing fun. We should probably get out more.

### Type guards

TypeScript has a lot of clever ideas, but none as clever as the type guard. The type guard is a way of saying to the compiler, look, I know more than you do. If *this* is true then we can assume that the type of *that* is *this*. It's an amazingly simple concept rooted in formal proofs. It requires you to do two things: track the status of all branches, and believe the developer; because if the developer lies the type is wrong and you get a run-time error.

We wrote the WA1 compiler in Rust, and we really missed type guards, so we put this in on a whim and then it became incredibly important. In this language, a type guard is a compile-time structure that lets you narrow a variable's type -- that is, the compiler will apply an automatic downcast to a variable that is subject to a type guard. That is often free, but is sometimes expensive, so we recommend you alias the guarded variable as soon as you can.

Here is the simplest possible guard, and a great example of a free one.

```
fn(x: Bool) => {
    if (x) {
        //in this block, x is no longer of type Bool, but is now of type true
    }
}
```

Type guards are, by design, very limited. They can only be applied to variables and consts, not members or functions or anything else. They can be encoded in the compiler, or, astonishingly, written by users. The `__typeguard` keyword is, of course, an unsafe keyword and needs to be in unsafe code.

```
fn __Option_isSome<T: IsAStruct>(x: __Option<T>) -> Bool __typeguard {
    true => __Some<T>
    false => __Null
} {
    x != __Null
}
```

Look, we're really not sure about this, OK? The thing is, though, they are kind of addictive. Once your compiler knows that `__Option_isSome` marks that your `__Option` is actually a `__Some` and downcasts for you when you access it so you can just call `__Some_unwrap` you get that kind of power thrill normally associated with a bloody knife on the Ides of March, you know?

But then it got worse.

### Ints

Let us make some observations about this seemingly innocuous data type. 

`Int`s are *hard*. Java dodged the issue completely by making a single 32bit signed `int` type, dooming their arrays to be 2 billion entries each. C++ has loads of the things, all badly named, and complains endlessly about the transformation from signed to unsigned. Rust has as many as C++ but at least named them well, but still has a lot of conversion rules. TypeScript inherits JavaScript's types, which are actually sane from a safety point of view (a single 64bit `BigInt` for use when you can't use a 64bit float) but are horribly space inefficient for tiny data.

The fact is, as soon as you use a fixed width integer, any arithmetic operator, any conversion between signed and unsigned, and any reduction of bits is a potential logic error that will have no run-time check and can ruin you. And yet if you want a decent speed for array indexing you have to use the damn things; and some algorithms exploit overflow and underflow so you can't get rid of them. We think they are a terrible idea for most maths, but we acknowledge they are needed if you need to, say, compute hashes.

So in a moment of madness, we made our `Int` a sorta-fake-generic type. We are not sure that this is a good idea, but bear with us.

Our `Int` takes two arguments; the min and max range. So this would be how you declared an `Int` that fits in a byte.

```
let var byte: Int<0, 255> = 16
```

The runtime will chose something appropriate: in this case, a 32 bit unsigned on the stack, and an 8 bit unsigned on the heap.

Because we are neither sadists not masochists, `Int` without qualifiers gives you a 32 bit signed int, which seems like a good default.

Ints can be widened for free; so you can always decrease the min or increase max because that is safe. Arithmetic operators will do this. (You cannot, however exceed the 64 bit limit, that's a compile error. We may lift that later.) 

```
let x1: Int<0, 64> = 32
let x2: Int<0, 64> = 32
let x3 = x1 + x2 // Int<0, 128>
```

You need to use conversions to go the other way, and of course we have a tool in our toolkit for this! The compiler will generate type guards for `Int`s if you use comparison operators.

```
fn(x: Int<0, 500>) -> Int<0, 31> {
    if (x > 31) {
        31
    } else {
        x //in this branch of the type guard, the type of x is Int<0, 31>
    }
}
```

We are the first to admit that *this is insane* but we have found that it works rather well in practice.

The problem now, though, is that this is (of course) wrong. An `Int` is not really a range, but is instead a set. By using bounds we are being pragmatic but not honest. A mathematically correct `Int` would look something like this.

```
let var a: Int<0-4 | 8-16> = 9
```

We... can't tell if this is even more madness or not. We imagine Haskell has gotten this right but we are too scared to look.

### The standard object model

The standard object model describes how objects are created, copied, and destroyed. It's an attempt at a pragmatic pass-by-value model.

#### Allocation types

There are three allocation types. 

Allocation type is a property of a type. That is, if a type is declared as a heap allocated object it can't change its mind without a recompile.

##### Stack

This allocation type lives completely on the stack. We use the multi value extension to copy things round. Stack allocated objects are 'flattened' - that is, if they themselves contain a stack allocated object this will be copied around. This is immensely pleasing, because it is our observation that most objects are very small and languages waste a lot of memory with pointers.

There are some limitations on stack allocated classes. 
* You can't have recursive definitions. 
* You can't have mutator functions. 
* You can't implement interfaces.

##### Heap by-value

This allocation type lives on the heap. It is allocated, the thing passed around is a pointer, and at some point it is deallocated. A heap allocated object holds a stack allocated object as an inlined set of members. A stack allocated object holds a heap allocated object as a pointer. 

It represents a pass by value datatype, which means that there will almost certainly be deep copies involved.

##### Heap by-ref
This is the scary one. After carefully making a pass-by-value language we throw it all away to make a pass-by-reference data type! This exists because we pragmatically acknowledge you will probably need it sometimes.

This allocation type also lives on the heap, and is almost identical to heap by-value. However, it is passed by value, meaning a copy will mostly be shallow.

You can't use type guards on these things - literally every function you call could mutate them, so you can't make any provable assertions. (Note, this may change later. See research topics, below.) You can, however, store these things in globals and as members in other `ref ptr` objects. (This last still needs some design.)

#### Type qualifiers

There are three type qualifiers. These are inspired by C++ cv type qualifiers. It is important to notice that these qualify the type, not the variable.

##### Const

For by-ref, this is the standard. Because it is the standard, it doesn't have any special keywords. When you move things around, you shallow copy. This means for heap allocations, this is just a pointer copy. For stack allocation, it's a full copy because that's all you can do, but if your stack allocated object contains a pointer to a heap allocated object, that is a shallow copy.

The by-ref heap allocation can't be const. There is no point having const for that - it exists to be modified.

##### Mutable

This means that you are passing mutable data around. We use the `mut` keyword.

```
let c: mut Thing = new mut Thing()
c.mutate() //this is fine
```

Passing by value of a mutable object means we need to worry about ownership (a strong const object can't be mutated, so as long as we don't delete it while it is in use, ownership is not an issue).

So any assignment of a mutable value object is a deep copy. 

```
let c = new mut Thing() //notice the type inference here
let d = c //this is a deep copy. d and c do not alias each other.
```

This means that a function 'owns' all the by value arguments passed in to it, and can freely mutate them.

Because deep copy is an expensive operation, we propose a copy ellison step, like C++. This replaces some copies with moves. A move is a shallow copy. The rule is, when copying from a temporary or a variable's last access, that copy can be turned into a move.

```
{
    let c = new mut Stuff   //move, from a function or new is always a move, because effectively from a temporary
    let d = c               //deep copy
    let e = c               //deep copy, but optimized to move by copy elision because last access to c
    e                       //deep copy, but optimized to move by copy elision because last access to e
}
```

We think that this reduces the cost of value passing significantly.

A copy from a const to a mutable is a deep copy for value allocation types. We don't track immutable objects, so we can't tell if it's a move or not. A copy from a mutable to const is tracked, and so may also be optimized by move elision. This is nice because it means you can use mutation for local construction of complicated objects, and then return them as const for no extra cost; this is something that Scala uses its builder pattern for (or used to, I think the standard library has been rewritten).

```
{
    let d = new mut Thingy
    d.mutate()
    let e: Thingy = d
    e
}
```

Type guards on value mutable objects only are in effect from the the opening `if` to the first time it is mutated.

For ref mutable objects, much of this does not apply. You don't get nice things with those. We expect you to manage your lifecycle.

##### Borrowed reference to heap value mutable

This is for short-term pass-by-reference mutable data. The intent here is to allow us to factor out mutator functions in such a way that we can mutate a thing over a single function call. The idea of borrowing is obviously, er, borrowed from Rust -- but the intent is very different. We are only tracking ownership in terms of understanding when something can be mutated, not when we can delete it. It is also even more restrictive than Rust's borrowing.

We use the `ref` keyword.

```
fn changeThatThing(a: mut ref Thing) -> Void {
    a.member += 1
}

//....

let mut a = new Thing
changeThatThing(a)
```

The data is shallow copied. This only works for heap allocated types (you could make it work for stack allocated types, but it would require boxing and so would be less efficient than naively correct pass-by-value functions). To make it into a value passed `mut` or a const is a deep copy.

A `mut ref` object has a a bunch of constraints on it to ensure the correctness we are applying.

1. You can only pass it down. You can't return it. (This is a lie, but we shall see more of this later.)
1. Calling a function that takes a `mut ref` counts as mutating it for the purposes of e.g. type guards.
1. You can't store these things, so they can't live as a member of an object, in a container, or in a global.

So... why would you ever use borrowed `mut ref` over heap by-ref? The type guard thing is useful, but the key detail is the creation. You don't create an object as `mut ref`, you create it as `mut`; and the `ref` bit just lets you take temporary ownership to mutate it as if you were mutating in place. On the other hand a heap by-ref is made that way. 

### Class declarations

Having armed ourselves with all the above, we can discuss class declarations. 

The language is a postmodern language: and so, as well as including functional elements, it includes object oriented elements. But we don't much care for full-blown OO, so we restrict it somewhat.

```
class heap Hello{
    constructor(
        //members...
    ) {
        //...
    }
}
```

Firstly, the constructor. We very much like the way Scala has a primary constructor, which defines the members of the class, so we stole that. We imagine we might have secondary constructors too. We will need a syntax to call the primary one.

The class has a storage declaration between the the `class` keyword and the name. This is one of `stack` or `heap`. We are slightly concerned that the `stack` keyword is longer, because we consider this to be the better option. In this case `heap` means emulated by-value. The by-ref case is covered further down.

Members are, by default, const. Const members are, by default, public. Here we declare two const members, one public and one private.

```
class stack HelloAgain{
    constructor(
        a: A, 
        private e: E, 
    ) {}
}
```

The `var` keyword means that you can change the value of that member, just like a `let var` variable. Also, just like `let var` the thing it's pointing to is const.

By default `var` members are private. 

```
class heap ChangeMe{
    constructor(
        var b: B, 
        public var g: G,
    ) {}
}
```

Because of the deep const nature of the language, you can only mutate `var` members if the object instance is mutable.

```
{
    let hi = new mut ChangeMe(new B, new G(1))
    hi.g = new G(1) //this is ok, because hi is mut

    let hiAgain = new ChangeMe(new B, new G(3))
    hiAgain.g = new G(4) //error! hiAgain is const
}
```

Member type declarations can have the `mut` type qualifier. Having a `mut` type qualifier has the same rules as a `var` member: it is private by default, and can only be mutated if the holding object is allocated as `mut`.

```
class stack ChangeWhatIHold{
    constructor(
        c: mut C, 
        var d: mut D, 
        public f: mut F,
    ) {

    }
}

{
    let t = new mut ChangeWhatIHold(new mut C, new mut D, new mut F)
    t.f.mutate() //this is fine because t is `mut`

    let u = new ChangeWhatIHold(new mut C, new mut D, new mut F)
    u.f.mutate() //error!
}
```

For heap by-ref classes, the syntax looks like this.

```
class heap ptr ThisIsTheScarySort{}

{
    //have to be allocated mut: this is always of type mut ThisIsTheScarySort
    let j = new mut ThisIsTheScarySort(...)
}
```

These are the only classes that are allowed to hold other heap by-ref objects. Obviously they can also hold regular by-value objects. See the research section below.

### Generic classes

In this language, unlike in Java, classes are a compile-time construct. This means a generic class is also a compile-time construct: it does not compile down to an erased class. It is really a template.

```
class heap Generic<T> {
    constructor(a: T, var b: T) {}
}
```

Stack classes can be generics - but you can of course end up with a recursive stack class by accident, so a stack class can refuse to be concretized based around what type arguments you concretize it with.

### Member functions

The simplest member functions do not mutate the object. All member functions are public by default.

```
class heap LookIHasFuncs{
    constructor(a: Int) {}
    fn simple() -> Int {
        this.a + 3
    }
}
```

Public member functions must use thin arrow. There is no technical reason for this beyond us wanting to make clear interfaces available to developers.

In a heap class, the magic local variable `this` is the pointer to the object. It is not a `var`; that is, you cannot assign to it. (We thought about `self` rather than `this`, but there is a theme of 
aping TypeScript in mundane syntax choices, so we went for that.)

In a stack class, `this` is obviously the data copied down the stack. The member access operator, '`.`', is simply choosing which stack entry to return. Syntactically, though, it looks the same.

```
class stack LookIHasFuncsToo{
    constructor(a: Int) {}
    fn simple() -> Int {
        this.a + 3
    }
}
```

Mutating functions look like this.

```
class heap MutateMe{
    constructor(var a: Int) {}
    mut fn addSome(r: Int) -> Void {
        this.a += r
    }
}
```

You can only have mutating functions on heap classes. This is because we are really copying the data in a stack class, and you would mutate a copy that would then be immediately discarded. 

The slightly weird `mut fn` syntax is, of course, because of the magic `this` argument. The type of the `this` argument is `mut ref`.

```
//This is a magical world which does not exist
class heap MutateMe{
    constructor(var a: Int) {}
    fn addSome(
        this: mut ref MutateMe, //error! 'this' is a reserved keyword
        r: Int
    ) -> Void {
        this.a += r
    }
}
```

This makes us think that we are not completely sure that having the `this` argument be magic is a good idea, but we discuss this later.

### Static member functions

Can we just say, we hate the word 'static'. It came from C where it originally meant 'global variable that is local to the function'. C++ adopted it to mean the thing we are talking about, presumably to reduce the number of keywords; and then Java did the same from C++. We know nothing about C# but we think it made it there from Java, too. 

We hate the name because it utterly fails to capture the intent, which is 'global variable or function within the namespace of a type'. In Java it is at least a bit more honest, because it also means 'function that conceptually doesn't use a vtable', but that is an implementation detail leaking into the language syntax in our arrogant opinion. We know we are not alone because Scala uses something completely different: the `object` syntax. We initially loved this but found that there was conversational ambiguity when you were talking about objects -- like, what sort of object? Rust is nice because by making `self` explicit in the function declaration it is easy to write both types of function. We would like to do that, but JavaScript `this` is so hysterically broken that we imagine programmers have a sort of wary expectation that `this` is always implicitly lurking, waiting to catch them in a pit trap, and we don't want to mess with that.

So our very provisional syntax is something like this.

```
class heap Hello {
    constructor(x: Bool) {}

    //it's a global so it has let
    shared let x = 300

    shared fn staticFunction() -> Number { 
        Hello.x + 4 
    }

    shared fn factory(x: Bool) -> Hello {
        new Hello(x)
    }
}
```

`shared` is a nice keyword for a couple of reasons. Firstly, it is the same length as `static`. This is important! Secondly, it is descriptive. We worry slightly that we might regret not bagging the `shared` keyword for multi-threaded stuff later.

### Alternative constructor syntaxes

As we typed this document, we started to dislike the `constructor` syntax. It's long, it's clumsy, it also doesn't give you much to differentiate the primary constructor from secondary constructors. Now that we have `shared`s, we started to explore alternative syntaxes. When you read the rest of this document, try and imagine these in the other places you see classes. 

#### Scala-like

In this one 'exposed' members are lifted to a named tuple after the class name. There is no body to the constructor. 'derived' members are declared in the body, and must be initialized using non-member function calls. These may only have 'exposed' members or previously initialized 'unexposed' members passed into them. You can also just randomly call functions in the body.

```
fn someFunction(x: Number) -> Number x * 2

class heap SomeClass(x: Number, s: String) {
    //this is a member derived from the other members
    private let y = someFunction(x)
}

let z = new SomeClass(5, 'hello')
```

This is much less wordy. Our experience with Scala has been, though, that derived members tend to be fiddly to implement because it is hard to create local variables: you have to jump out to a function and this means it is hard to share state between the initialization of two variables within the body of the class.

#### Rust-like

The Rust solution to this is similar: the object is broken down into a record and factory functions. This means you can always create a 'raw' object by using the (lovely JavaScripty) syntax of record creation. We like this, but we don't like the way there is no way to hide derived members. So our version looks like this.

```
fn someFunction(x: Number) -> Number x * 2

class heap SomeClass private(x: Number, s: String, private y: Number) {
    //note that `new` is no longer a keyword
    shared fn new(x: Number, s: String) -> SomeClass {
        SomeClass{x, s, y: someFunction(x)}
    }
}

let z = SomeClass.new(5, 'hello')
```

Here `private` in front of the members tuple means only the class can do the record construction syntax. It's optional, so if you omit it you get to use the record syntax everywhere.

This is wordier than the Scala solution, but we prefer this one.

### Traits again

A long time ago we mentioned traits. Remember them? If you can recall, a trait is not a type: it's a contract that a type can adhere to, inspired by Haskell's type classes. Here is an example of one. 

```
trait NumberThatCanGoUp{
    fn add(x: Int) -> Int
    fn copy() -> TraitOne
    mut fn inc() -> Void
}
```

Traits can be implemented by classes. Because they are like type classes, they are implemented outside the class. However, because that turns out to be a huge pain in the bum in Rust, if the class happens to implement the functions described in the trait, the trait implementation can just use them. This is a pragmatic decision. Perhaps we will regret this later.

```
class heap TypeOne{
    constructor(x: Int) {}

    fn copy() -> TypeOne {
        new TypeOne(this.x)
    }

    fn add(x: Int) -> Int {
        this.x + x
    }
}

class TypeOne implements NumberThatCanGoUp{
    mut fn inc() -> Void {
        this.x += 1
    }
}
```

A trait implementation does not have access to private members of the class, otherwise you could trivially break encapsulation.

Traits can have type arguments. They must be passed into the trait from the class when it is implemented.

```
trait Wrapper<T> {
    fn get() -> T
    mut fn set(t: T) -> Void
}

class heap Storer<T> {
    constructor(var t: T) {}
    fn get() -> T this.t
    mut fn set(t: T) -> this.t = t
}

class Storer<T> implements Wrapper<T> {}
```

### Inheritance

So far every function call has been known at compile time - 'static' in Java/C++ speak. It is useful to have function calls that are not known at run-time - 'virtual' in C++. We use interfaces for this. Interfaces are very similar to classic Java interfaces.

An interface looks very similar to a trait.

```
interface HelloThisIsMe{
    fn someFn() -> Int
    mut fn letsChangeIt(x: Int) -> Void
}
```

The first big difference between an interface and a trait is that an interface is a type, whereas a trait is not. So, you can pass them as arguments to functions, store them as members of objects, declare them as variables and so on.

```
fn (a: HelloThisIsMe) -> Int {
    //...
}
```

The second is that a class implements an interface as part of its declaration, rather than externally like a trait. In the MVP only heap classes can implement interfaces. This is so the interface is a predictable size on the stack.

```
class heap SomeConcreteClass implements HelloThisIsMe & SomeOtherInterface {
    fn someFn() -> Int {
        //...
    }
    mut fn letsChangeIt(x: Int) -> Void {
        //...
    }
}
```

The relationship between a class and an interface is 'is-a'. The relationship between a class and a trait is 'has-a'.

At runtime a class that implements an interface always carries around a vtable pointer. This is always the first member of the class. That means that any call to a function indirects to the vtable. (A research topic is passing the vtable pointer with the `this` pointer, so that an interface is two pointers big.It may perhaps be better.)

We have seen Java code where every class has an interface associated with it, presumably to reduce cognitive load on the callers of the class. Please, we beseech you: don't do that here. C++ does that because it has to. If we think some sort of class api syntax would be useful, which looks like an interface but has no runtime cost, we could do that.

Interfaces can extend other interfaces. Classes cannot extend other classes.

```
interface Base{
    //...
}

interface Derived extends Base & OtherBase {
    //...
}
```

Have you noticed how we are using `&` to show we are deriving the intersection of the base classes? Yes, we think that's very clever too, thank you. Could you derive the union of types? No. Maybe not so clever then...

### Arrays

Arrays are allocated on the heap. Our arrays are resizable if they've been allocated as mutable. Arrays follow the same standard object model rules as other objects, with shallow copies of immutable and deep copies of mutable ones. A resize may cause a copy of the elements as the underlying memory changes. That is always a move, so always a shallow copy.

We have given a lot of concessions to TypeScript syntax, but we draw the line at that horrible square bracket syntax for array type declaration (`a: number[]`).

```
let a = new mut Array<Int<0, 255>>
a.push(1)
a.push(2)
```

Array literals are declared with square brackets. Array literal types are quite simple. We don't have any fancy type guards like we do with ints.

```
//literal is of type [1, 2, 4, 8, 16]. a is of type Array<Number>, which is obviously an acceptable widening.
let a: Array<Number> = [1, 2, 4, 8, 16] 
//both b and literal are of type [true, true, false]
let b = [true, true, false] 
//the literal is of type [0, 1, 1, 0]. The variable is of type mut Array<Int>. This means a deep copy from the literal.
let c: mut Array<Int> = [0, 1, 1, 0] 
```

It's worth discussing an interesting optimization here. Strong and deep const means we can pre-compute array literals and put them into WebAssembly memory, so that instead of an allocation and assignment it's ready out of the box. This is quite interesting. We imagine we can probably optimise quite a lot of literal expressions this way.

Arrays have a length and a capacity. We don't need the capacity in immutable arrays, but having it means we can avoid a deep copy from an owned mutable array to an immutable one (because the memory layout would be different) and we think that might be quite common.

Under the covers, an array is an `__Array`, which is a fixed size unsafe array, plus a length and capacity. We can't decide whether to pass the array around as a triple (length, capacity, pointer) or as a single pointer and do some l33t h4x0r pointer manipulation on the allocated buffer. Both have their upsides. One thing we are not going to do is expose the low-level unsafe array outside of unsafe code.

### Iterators

Our iterators are deliberately underpowered. An iterator is an incredibly unsafe view of the internals of an object. For const objects, this is not a big deal - you can't do anything bad with it. For `mut` objects, this is essentially a pointer to the internals, and it breaks a lot of the ownership rules.

So we took a very limited view on iterators. Here are the iterator traits.

```
trait IsIterator<T> {
    fn hasNext() -> Bool
    mut fn advance() -> Void
    fn get() -> T
}

trait IsIterable<T, I: IsIterator<T>> {
    fn iterator() -> mut I
}

trait IsVarIterator<T> extends IsIterator<T>{
    mut fn set(t: T) -> Void
}

trait IsVarIterable<T, I: IsVarIterator<T>> {
    mut fn varIterator() -> mut I
}

trait IsMutRefIterator<T: IsHeap> extends IsIterator<T>{
    fn getMutRef() -> mut ref T
}

trait IsMutRefIterable<T: IsHeap, I: IsMutRefIterator<T>> {
    mut fn mutRefIterator() -> mut I
}
```

We can only mutate heap classes. We use the built-in trait `IsHeap` to make sure that only containers that contain heap allocated objects can provide implementations of `IsMutIterable`. There is currently a syntax gap as to how a generic class conditionally implements traits, but I don't think it should be so hard.

Note that the `IsMutRefIterator` returns a `mut ref T`. This can only be written or called in unsafe code. It means that these iterators can only be used implicitly in `for` loops, which manages the lifetime of the iterator for you. The other iterators can be used freely, by calling `iterator` and `varIterator` yourself.

There is no iterator that returns a `mut T`. What would be the point? You can't change the array member, you are just mutating a copied value. 

Here are some examples of the loops in action.

```
let arr: Array<Number> = [1,2,3]
for(let it of arr) {
    let var n = it.get()
    n *= 3
}

let arr2: mut Array<Number> = [1,2,3]
for(let it of arr2) {
    let n = it.get()
    i.set(n * 3)
}

let arr3: mut Array<Thing> = [new Thing]
for(let it of arr3) {
    let n = it.getMutRef()
    n.mutate()
}
```

As it should be fairly obvious, a `for ... of` loop is just a macro. `for` loops are just macros in general, really, but we don't go quite as crazy as Scala's for comprehensions. Here is what the code unrolls to.

```
let arr: Array<Number> = [1,2,3]
{
    let var it = arr.iterator()
    while (it.hasNext()) {
        let var n = it.get()
        n *= 3    
        it.advance()
    }
}

let arr2: mut Array<Number> = [1,2,3]
{
    let var it = arr.varIterator()
    while (it.hasNext()) {
        let n = it.get()
        i.set(n * 3)
        it.advance()
    }
}

let arr3: mut Array<Thing> = [new Thing]
{
    let var it = arr.mutRefIterator()
    while (it.hasNext()) {
        let n = it.getMutRef()
        n.mutate()
        it.advance()
    }
}
```

We figure the iterator type from the type of the iterated object. Given there is no runtime cost to this, this feels safe. I have the feeling we may need a fourth iterator type, that combines the `var` and `mut ref` flavours.

### By-ref arrays

Arrays as they stand are by-value; this means the same copy rules apply as by-value heap objects. We expect there will be call for by-ref arrays too. This is conceptually very easy but requires some syntax. We haven't done that yet.

### Linking

Of all the things we love about TypeScript the one we love the most is the vanilla `import` syntax that it inherits from es6. We like that we don't have to worry about getting com.robots.factory.model.core.interfaces.singleton.Maker correct, and the very simple aliasing rules. Having said that, they are, frankly, far too over-featured for our liking. So we took the bare minimum we could and designed the linking system we have always wanted.

To export something from your file you use `export`. 

```
export fn thisIsGlobal(a: Int) -> Int {...}
```

The full name of this exported symbol is `<project>.<file>.<symbol>`. So this means that we treat the files in your project as a flat namespace. We woke up in the middle of the night fretting about this a bit, but then we decided that actually we didn't care.

Symbols are encapsulated in their files: you can't get at them if they are not exported. We remember the despair that was caused when Java's Project Jigsaw was introduced and we want to get ahead of that right now.

Import syntax uses `import`.

```
import {*} from '../somewhere'

somewhere.overTheRainbow()
```

Importing is not macro substitution. Come now, we are not savages. We have two passes across our code files: the first is a very quick skim that parses exported declarations and puts them into a json file. The second is the full compile which reads these export declarations when an import directive is encountered. When you import a symbol from a file, it appears in your namespace as `<file>.<symbol>`. We like the idea of the `as` syntax to locally rename `file`.

```
import {*} as 'whatever' from './folder/somewhereElse'

let youNeed = whatever.youWant()
```

Once these have been built, we compile down to WASM object files and use the LVM liker to link; we then get a final .wasm file. This is amazing! It means we can then run whole-program optimizations using the standard tooling. We really love WebAssembly.

This falls down somewhat when you want to expose stuff from your project. We don't want `export` to mean 'you can see this externally' because then we can't eliminate dead code. So we have two proposals; not sure which is best.

#### An entry point

In this model we have a special entry point file (by default called 'index', natch) and when you export stuff from that it's called `<project>.<symbol>`. This works great, so long as you don't want to share these symbols internally, and then it gets a bit horrible. How do you import? We have a couple of options - the first is the slightly crazy `export import` syntax, used only in the entry point file. 

```
export import {thingToExpose} from './someInternalFile'
```

The idea here is you don't declare anything you want to share internally in the entry point; but you do have a mechanism for easily exposing stuff declared elsewhere. This is quite nice, but it's not completely clear that it's technically feasible; and it feels very modernist, in a bad way. (It is obviously modelled on Jigsaw which in turn was modelled on exposing symbols from a dll.)


#### Another keyword

In this model we don't have an entry point file. When you export a symbol from the project you need an extra keyword. 

```
export global class heap WowThatsALotOfKeywords{
    //...
}
```

Maybe there are better keywords? But the point is, internally this is still known as `<project>.<file>.<symbol>` and externally `<project>.<symbol>` because it's not helpful to expose the internal file structure. This is nice and messy, with no irritating modernist entry point. But the syntax is definitely worse; and it is also not completely clear it's feasible.

### Strings

Ok, look, here is another rant. You thought integers were hard? Strings are harder. Some people labour under the delusion that strings are lightweight. My friend, nothing could be further from the truth. Strings are complicated, scary objects.

If you are somehow surprised by this statement, it is worth stopping at this point and reading something on the topic. We suggest [this blog by Manish Goregaokar](https://manishearth.github.io/blog/2017/01/14/stop-ascribing-meaning-to-unicode-code-points/). If that doesn't scare you, you should read up about Unicode normalization. If *that* doesn't scare you, you are probably a Unicode board member - to which we say, stop reading this and implement superscript 'q'!

Anyway, our strings are passed by value. We propose that they be UTF-8 NFD. We dislike `+` for string concatenation because addition should be commutative, and string concatenation is not, so we won't do that. Honestly the template syntax that ECMAScript introduced recently should be enough, although we have a soft spot for Lua's `..` operator. The string libraries will be based on the stuff that Rust does because it tries really hard to get it right.

Because of all this special syntax, strings need to be baked pretty hard into the compiler. We don't want to be clever here and if we have to do a ton of special casing then so be it, strings are hard. Having said that, take a look at the research topics.

Internally, strings are probably an `__Array`, a buffer capacity, a byte length and a grapheme cluster count (the number of 'characters' in a string, and therefore the length). We won't have a character type because that just isn't a thing in Unicode: instead we will have strings which contain one grapheme cluster. (These may well be multiple unicode code points. A great example from the above blog is the family emoji 👩‍👩‍👧‍👦 -- this is seven Unicode code points! A lot of editors and browsers get this wrong.) If we do a square bracket operator (`str[i]`) that will just return an option on a string.

### Tuples

Tuples are immutable stack objects. They need special compiler work because of the `()` syntax that everyone expects tuples to have. (TypeScript uses `[]` for tuples, but that's just syntactically wrong, and in this language an array and a tuple are very different under the covers.) 

The type of an n-tuple is `Tupleₙ<T₀, T₁... Tₙ>`. We will generate these types at compile time as needed -- we have to, because we don't have vargs in our generics. We think we can probably use magical macro type substitution so that instead of having to write the full type name we can use round brackets.

```
//This function that returns a 2-tuple
fn quotientAndRemainder(x: Number, divisor: Number) -> (Number, Number)

//...is the same as this
fn quotientAndRemainder(x: Number, divisor: Number) -> Tuple2<Number, Number>
```

It's worth talking about the 1-tuple. (By the way: did you know this is called a monad? Well, we didn't, we only found out researching for this. It's a pity that we didn't know this when it was cool to not understand monads.) The 1-tuple is a nightmare! It is stupidly ambiguous. With a bit of parser badness you can interpret `(1,2)` as an instance of the 2-tuple. (We deliberately don't have the comma operator to make this possible, by the way.) But `(1+2)` -- what does this mean? You can legitimately consider this to be either the type `(3)` or the type `3`. In other words, The brackets are there to construct a 1-tuple, or they are they for precedence reasons.

We decided that the 1-tuple will only be really needed for meta-programming. So there is no macro for it, and you need to type out the full type.

```
let x = new Tuple1(3)
```

It's worth reading the research notes on tuples for more tuple madness, by the way.

### Lambdas

#### Basic implementation 

A function declared inline is a lambda - that is, it can see the scope it's in. What that means depends on the allocation type of the thing.

The simplest case is const. This is easy, because the lambda can just examine the object, safe in the knowledge that all it has done is added a gc pointer.

```
{
    let a = new Thing
    let f = fn() => a.someFunction()
    f()
}
```

Your mental model of a lambda should be a packet of data, along with a single function. The packet of data is initialized by statically analyzing what part of the outer environment the lambda calls, so you end up with something that is semantically this.

```
class heap Packet {
    constructor(private a: Thing) {}

    fn apply() => {
        this.a.someFunction()
    }
}

{
    let a = new Thing
    let f = new Packet(a)
    f.apply(packet)
}
```

This semantic model is important because it dictates how we deal with the other allocation classes. For mutable by-ref, aka `heap ptr` classes, it is also easy. You need to annotate the lambda as a `mut`.

```
{
    //this is a class heap ptr, a mutable by-ref class
    let a = new mut CrazyThing
    let f = mut fn() => a.someCrazyFunction()
    f()
}
```

This is equivalent to this.

```
class heap ptr Packet2 {
    constructor(private a: CrazyThing) {}

    fn apply() => {
        this.a.someCrazyFunction()
    }
}

{
    let a = new mut CrazyThing
    let f = new mut Packet2(a)
    f.apply(packet)
}
```

(Is that right? Should it be `ptr` rather than `mut`?)

So far so good. But what about mutable by-value? The problem with those is if you take a reference to one of them you end up with a deep copy -- they are by-value, so you can't reference them. But our old friend `mut ref` comes to our rescue. 

```
{
    //this is a class heap, a mutable by ref class
    let a = new mut Thing
    let f = mut ref fn() => a.someMutatingFunction()
    f()
}
```

This means that we have taken a borrowed mutable reference to `a`. We are bound by the same rules for `f` as we are for all other `mut ref` things: we can't store it, and we can't return it from a function, we can only pass it downwards. In other words, it can't escape up from this scope. From the moment it is created to the moment it is destroyed, just like a `mut ref`, we can't trust the thing that `a` points to, so type guards stop working.

The equivalent code is, of course, a class that holds a `mut ref`; but because that's illegal, this has to be generated by the compiler.

```
class heap Packet3 {
    //error! May not have a member of qualifier `ref`
    constructor(private a: mut ref Thing) {}

    fn apply() => {
        this.a.someMutatingFunction()
    }
}

{
    let a = new mut Thing
    let f = new Packet3(a)
    f.apply(packet)
}
```

This is, of course, Rust ownership-lite. The lambda (like the iterator earlier) has taken a reference to an object and we use the compiler to track it. Unlike Rust there are no explicit ownership scope rules, so we are very conservative so we don't get it wrong.

#### Type

The type of a lambda is made up of its argument types and return type. In practice this means that a lambda is a virtual function call. So our temporary classes also implement a well-known interface, synthesized by the compiler. Because types are entirely compile-time we can erase this and the only people who know are us. This means we can now treat lambada as special syntax for interfaces. 

```
fn mapSum(a: Array<Number>, f: fn(Number) -> Number) -> Number {
    let out = 0
    for (it of a) {
        out += f(it.get())
    }
    out
}
```

This, then, compiles to this.

```
//This is defined by the compiler; it probably won't be called this
interface LambdaNumberNumber {
    fn apply(x0: Number) -> Number
}

fn mapSum(a: Array<Number>, f: LambdaNumberNumber) -> Number {
    let out = 0
    let var it = a.iterator()
    while (it.hasNext()) {
        out += f.apply(it.get())
        it.advance()
    }
    out
}
```

We would hope that we are smart enough to build our iterator so that it can be inlined, and the whole thing then collapses down to a nice loop doing pointer maths.

### Enums, `switch`, Options

#### Enums
Rust enums are utterly amazing. Essentially they are discriminated unions, and you can just use the discriminator on its own to get what most languages call an enum, or you can stick data in them too. They have some restrictions -- they can't be recursive, for example -- but they are a clean and easy way to do polymorphism. 

We are not proud, so we steal this concept wholesale. 

```
enum ThisIsAnEnum{
    FirstThing
    SecondThing {
        constructor(a: Number) {}
    }
}

{
    let b = ThisIsAnEnum.FirstThing
    let c = ThisIsAnEnum.SecondThing(5)
    //this is fine because c is of type ThisIsAnEnum.SecondThing
    let d = c.a
}
```

The enum lives on the stack... sort of. This is harder than in Rust because that just stack allocates a block of memory corresponding to the largest size an enum element can be, and does pointer casting. We don't have stack allocation, so we need to allocate slots for each data type and the discriminator. That means if we have, say, an enum where one element has an i32 and one has an f64, we will need to create stack elements for both, because Wasm has strong typing at the instruction level. Obviously we can share this if multiple members have the same types, but it means our enums will be larger than Rust's.

In terms of typing, the enum has a type, obviously; the members have a type, too, and are `<enum name>.<enum member name>`. So you can, if you want, declare a function to have an argument that is typed to an individual enum member. We think this may be more useful than it sounds because it will mean being able to break big switch statements up.

It goes without saying -- no actually, it doesn't so let's say it -- that all the members are deep const. You can't have a by-ref heap classes in an enum. This again could change - this puts a very nasty constraint that we shall see later.

#### `switch`

On that topic, `switch` and `enum` go together like I don't know designing programming languages and angst. So let's do them together.

Not for us the classic C `switch` statement that made it through even into Java. Guys, why did you even do that? No, this is a `match` statement in `switch` clothes. That means we don't have break or falling through, it's a branch to a block. And, it's an expression, of course.

The simplest is a switch on value. Here is a completely stupid example. Notice fat arrow doing double duty on the branches. This is supposed to show that the return type of the switch is being implied from the branches, but honestly it looks a bit wonky.

```
let a: Number = getSomeNumber()
let b = switch (a) {
    4 => 1
    5 => {
        //this doesn't need to be a block but here it is to show you
        a + 9
    }
    //default. Should this use the default keyword?
    _ => 0
}
```

If you have runtime type information, you can switch on a type. Only enums have that so far. Switches on types look like this. Yes, this is a type guard, so you can get at the inner members.

```
switch(c) {
    c: Enum.Member1 => {
        c.memberOfMember1
    }
}
```

It's important to understand that Rust and Scala both work very differently to this: they pattern match the members whereas this is a type narrowing based on the discriminator. This is much more like TypeScript. A pattern match could well be possible too, it's just not in the MVP. 

#### Back to enums

As you probably guessed, an enum is a macro for a stack-allocated class with a magic discriminator member and weird aliasing rules for its other members. It's really the stack based answer to interfaces. Because of the casting, you couldn't implement it in user code. But, you can get jiggy with some other things. Let's build up to something you might recognize. 

First, enums can be generics and have member functions.

```
enum ThisLooksFamiliar<T>{
    None
    Some{constructor(val: T) {}}

    fn isSome() -> Bool {
        this.discriminator == ThisLooksFamiliar<T>.Some
    }
}
```

That's nice, isn't it? The constructor syntax is clunky but honest. You can't have shared data members, but that might be a possibility.

Next, the `discriminator` member. This is a semi-magical member. It's actually not magic at all in some senses: it really exists on the enum and it really is called that. It's the one common member, which is why other common data members might be possible. It is magic in the sense that it's not in the constructor of the members. It is also magic in the sense that you can never ever change it. It has another powerful piece of sorcery, which we will see later, but which should not be a surprise.

Now let's go ceh-ray-zee.

```
enum IHaveSeenThisBefore<T>{
    None{
        fn map<U>(f: fn(t: T) -> U) -> IHaveSeenThisBefore<U> {
            IHaveSeenThisBefore<U>.None
        }    
    }
    Some{
        constructor(val: T) {}
        fn get() -> T this.val
        fn map<U>(f: fn(t: T) -> U) -> IHaveSeenThisBefore<U> {
            IHaveSeenThisBefore<U>.Some(f(this.val))
        }
    }
}

enum IHaveSeenThisBefore<T> implements SomeTraitWithMap<T> {
}
```

Look, yes, this is an Option. Because enums are stack classes, and they can't contain `heap ptr`s, you can't have Options on `heap ptr`s. This is obviously bad. There is a potential solution to this, in the research.

Now let's talk about the other, more interesting parts of this. If you write a function in every member of an enum which has the same name and the same signature (and yes: same name, different signature is a syntax error) the compiler synthesizes a function that inserts a switch for you.

```
enum IHaveSeenThisBefore<T>{
    //what we had previously...

    //This function is generated by the compiler. It's a static function so 
    //we can strip it out later if it's not used.
    fn map<U>(f: fn(t: T) -> U) -> IHaveSeenThisBefore<U> {
        switch (this) {
            x: IHaveSeenThisBefore<T>.None => x.map<U>(f)
            x: IHaveSeenThisBefore<T>.Some => x.map<U>(f)
        }
    }
}
```

The way that works, by the way, is a typeguard: but actually, it wouldn't work that way, because the compiler can insert the switch on the discriminator and cast, but there isn't a syntax for that. That's why the syntax looks so wonky.

Of course the result of this is that we have satisfied the requirement of `SomeTraitWithMap` (Functor? I can honestly never remember) and so we can call a good old `map`.

### Modules

We talked about the linker. One thing we didn't talk about was library deployment. We -- you will no doubt be astonished to learn -- have opinions on this. This is a topic as important to a language's success as things like bracket conventions and casing: i.e. very. In some ways it's more important because it is the first barrier to entry to new developers who want to start.

Anecdotally, package repositories are expensive to host. This has caused us some heartache because while we don't expect to make any money from this we certainly don't want to lose it. Another thing to consider is a particular problem for JavaScript: this is the attack vector of inserting a package into npm which has not been generated from the underlying open source code, so you are running something with nasty things and don't know it. We think this is a bad situation and we want to avoid it. Lastly, we will be honest, we think the idea that downloading pre-built code is a new-fangled thing and we got on just fine by weeping over makefiles. Get off our lawn!

So we want to build a module system from code downloads, where you just download the code and build it yourself. Because we hate that node_modules folder with a passion, we will do what Yarn, Rust, Maven and all other sane package managers do: and have a central cache of code on your machine. We will download and compile code with the given compiler version to produce compiled objects that can be linked. We imagine this means we will need to depend on something like a repo name plus a commit identity. We think Go does something like that but we are not that familiar with Go.

We will need some kind of package description file. We like npm's way of doing semantic versioning dependency and will almost certainly steal that. Module dependencies will either be public or private. A public dependency is a singleton, and so shared with all other modules in the runtime, so all the module's version numbers of this have to agree or you get build errors. This is because we have seen the horrors of Java programs shipping like five versions of a logging library. 

However, you can have a get-out clause of having a private dependency. This means you and you alone see this instance of this module. This essentially works by telling the linker that the module is a different name. We think? If it stores data, sucks to be you if you think you can share this. But! We can actually tell this fairly easily from our code analysis, so maybe we can warn. We know that private modules will be abused, but we also know that on the server where people don't care about executable size this sort of thing is just a fact of life.

### I can't believe you read this

Really! You crazy person! Thanks!

## Research notes

These are topics that have been set aside for a later release. Some are true research topics, and will involve mock ups and reading more papers. Others just involve reading up how other languages do them. Occasionally they are just primal rage turned into markdown.

You don't need to read this to understand the language.

### Trait and interface default implementations

For the MVP we don't have trait default implementations. That is, we can't implement functions in a trait. Most languages do this. We should probably do it too.

We also don't have interface default implementations. These would be functions that the class pulls in when it implements the interface. If there are multiple implemented functions with the same name, this is an error, unless you override it yourself. This is more contentious and it took Java ages to do it. Probably should?

### Trait and interface data members

We actually could do trait data members pretty easily. It is, after all, just another contract that the type adheres to.

```
trait SomethingMaybe{
    a: Number
}

class stack AThing {
    constructor(a: Number) {}
}

class AThing implements SomethingMaybe {}
```

The difference is that if you wanted to implement them outside the class in the true type class way, you'd need to fake it. Then a regular member is syntax sugar for a `getX()` function, and a variable member also has a `setX()` function; and the code inserts those. It means yet more special casing and parsing. Worth it? Maybe?

```
trait SomethingMaybe{
    a: Number
}

class stack AnotherThing {
    constructor(b: Number) {}
}

class AnotherThing implements SomethingMaybe {
    //magical parsing of get<X> fn means this satisfies the readonly trait data member
    fn geta(): Number {
        this.b
    }
}
```

You can do a similar thing for interfaces but means yet more special casing.

### Trait functions with extra type conditions

Haskell does this and it is rather nice. It basically says that some functions in a type class need extra trait constraints to be satisfied before they show up. It might simplify some of the iterator stuff - that is, you could have one `IsIterable` trait with multiple `getX` functions, each one of which has a constraint. Maybe we should figure the syntax out now and implement this feature later.

### Reflection

We have no reflection. If we introduce reflection we need to make sure it doesn't break module or data encapsulation, or const correctness. That means we will be only exposing a subset of members and functions: the subset that you would be able to see at compile time. What does that mean? Does such a language exist? Java allows you to break all encapsulation and it is without question the very worst aspect of that language.

### Operators

There are no user defined operators in the MVP. (This is actually not quite true - we will probably be writing the `Array` class in user code, so we will need to be able to describe the lvalue and rvalue versions of the index operator, `a[i]`. We will use the techniques below for that, probably.) But there are no user-defined operators the way, say, Scala does them.

It's worth quickly talking about Scala. The Scala operator system is really nice from a technical point of view - everything turns into a function, and the function name is encoded using some some sort of mapping from funny char to alphanumeric characters (so `++` is something like `$plus$plus`) so it can still be represented in Java; and the compiler substitutes this:

```Scala
a ++ b
```

for this:

```Scala
a.$plus$plus(b)
```

so anyone can write any operator they like. And they did!

And so the standard library was filled with `:/|-@***=` and when normal developers tried to write code, they had no idea what was going on, because you just can't google for this stuff. This caused something of a pushback from the Scala community -- and so there was very much a rowback from using operators to using simpler text-based function names. 

(It's weird, though -- Haskell has a similar set up and no one grabbed pitchforks to storm that particular castle. We have opinions, but here is not the place. The sad moral of this story, though, is that this is as important to a programming language's success as to whether or not it has a sound typing system -- probably more, look at JavaScript!)

Having said all that, we have had modest success with operators. It might be that we are misty-eyed about our C++ days when Stroustrup fearlessly made the bitshift operator a string mangling thing (we confess at this point that the first time our young C-trained minds saw this we were scared. What dark wizardry is this? How is he concatenating strings by bitshifting them? It it exploiting some overflow property of `char` arrays that we did not know existed?) that we think we should keep the baby and some amount of bathwater. In particular we had a lot of success using Scala to write out formulas, by providing functions called things like `²`, `³`, and `√`. It makes it easy to scan maths and make sure you transcribed it correctly, and we claim we reduced our bug count significantly.

So, we propose the following rather modest, post-MVP proposal, that still needs fleshing out.

We will have a predefined table of operators. Unlike Scala, we map semantic names. So, the square operator `a²` is always postfix; has a fixed precedence; and always substitutes for `a.square()`. Our operator table is scooped from common maths symbols, and is purely Unicode; we will be very conservative. Operators can never mutate underlying data, so if you call `container ∋ elem` that will map to `container.contains(elem)` and `contains` will have a non-mut signature.

This has the advantage that most operator substitutions tend to be geared to the convenience of the writer. These are geared to the convenience of the reader. Unlike `:+`, you can google for `∋`, but it is a pain to type.

More research is needed to build the table and precedences.

### Function purity

The deep const nature of the language allows us to make assertions about function purity with absolute certainty. A function that only reads from its inputs and global consts is truly pure because nothing can go and muck with the global. This means we might be able to mark functions with various levels of purity. We think this can include things like reading and writing to mutable globals, certain operations we flag specially (like file reading and writing), interacting with threading primitives (yes, we don't have any but maybe we will one day), mutating by-ref arguments, and all that other stuff.

This is a research problem because it seems easy but falls apart quite quickly. What do you do with interfaces? Lambdas? How do you record it? And, most importantly, what do you do with it when you have it?

### Tail recursion

All the cool kids have it. We don't think it's that important for a language with good ol' fashioned while loops, but we want to be a cool kid too and it's one of those things that sometimes makes a huge performance difference to mundane code, so let's stick it on the list.

### Type guards and mutability

Right now we take a no-prisoners approach to mutability and type guards. If a type is mutable, we bale and say we can't prove anything, so the type guard has no effect. This is actually sub optimal! There are functions that definitely don't mutate their arguments and we think we can probably do a little better than that. It involves tracking object lifetimes which we think we can do a bit in a naive way. For example, our rules around operators mean we know which operators mutate their operands. There is probably a much deeper and better way, though.

### Commas can trail, commas are optional

Look can we just have this conversation? Don't go.

Trailing commas are really, really useful because they reduce commit noise when you add extra arguments to multiline functions. If all function arguments have a comma at the end, then you don't need to insert a new one on the line above. (If you use leading commas, you disgust me. Get out of my house, and never darken my door again.)

But better than that is... optional commas.

I actually saw this discussed on the Scala contributors list, and once you have witnessed the heresy it's hard to forget. The idea is that a comma is like a semicolon: you can use a return character instead. It makes the language feel lighter and more scripty. Here is a function with long arguments.

```
myBestestFunction(
    longArgument
    anotherLongArgument
    goshICanKeepTyping
)
```

BURN THE HERETIC! 

This feels very much like a feature you need to build in right at the start, so this probably should be one of the first research topics looked at.

### Type of a constant object with the default constructor

We wrote this document by writing the headers first and then filling the contents out later. This one meant something to us once upon a time. No idea now. We are getting old.

### Mocking at compile time

One of the things that we have seen in our long time proverbially hammering screws into planks of wood with spanners is the many ways that people take beautiful languages and make them awful to write tests. Like have you seen Java injection? Even the name is terrifying.

But the fact is, it is very useful to be able to stub bits of your code out to write unit tests. And doing that means you have to de-static (this is a technical term we just made up) your architecture so that it accepts either the real version or the mocked version. This then makes the whole program a mass of virtual functions and it is much harder to do compile-time optimizations.

So: research topic. How can one produce a unit test (which should be a standalone executable) by taking a set of source files from a project and replacing some of the imported types by stubs, so you don't need to use dynamic types in production? Must be possible, right? Java's very clever JIT does a good job here (it does tricks like only making function calls virtual when a class's subclass is loaded) and we feel like we need an AOT response!

### Interfaces are a pair of pointers

So far, heap classes have been passed around as a single pointer. If they inherit from an interface, their first data member is a vtable, a pointer to a shared table of function pointers for the member functions. This is kind of irritating. Because classes don't inherit from each other, a class doesn't need the vtable: the only time it needs a vtable is when it is widened to an interface. So why not have the interface be a pair of pointers? We would move the vtable out of the class and onto the stack. 

It's not clear that this is an optimization, though. It reduces the size of our classes, but increases the size of pointers to our classes, meaning things holding interfaces are bigger; and, importantly, if you have a lot of pointers to a small number of classes your memory increases significantly. More research is needed.

### Immutable arrays don't need capacity

Our Array type has a capacity, a length and a pointer. It's either a triple that we pass around on the stack or it's a single pointer to a buffer that we manipulate to get those two values.

Thing is, we don't need a capacity for an immutable array. Like, what are you going to do with the extra memory? We mention that it's nice to have so we don't need to do a deep copy from a mutable to an immutable, but what about the case where an Array is a stack-passed triple?

In this world, a mutable array is a stack-passed triple but an immutable array is a stack-passed pair. This seems fiddly as heck to implement, frankly, because we'd need to have two different run-time types for one compile-time type. But... one `size_t` back!

### Tuples as function arguments; Void as the zero tuple

Strictly, mathematical functions consume one thing and transform it to another. Even more strictly, from Wiki: 'a function is a binary relation over two sets that associates every element of the first set, to exactly one element of the second set.'. If your thing is multivariate, you use a tuple: so in the formal definition, either or both of the two sets can be sets of tuples. This means a function can map from a tuple to a tuple if it wants.

As programmers we are so conditioned to accept the syntax of the form `f(x,y)` that we don't think too hard about the fact that it's the function `f` applied to the tuple `(x,y)`. (Yes, Haskellers, you can look smug now.) It's particularly hard because the stupid formal notation should really be `f((x,y))` but as ~~mathematicians are fundamentally lazy~~ maths is written for people, not machines, this is always shortened to the syntax we know and love.

This is slightly painful because it would be nice to be able to take a lambda, and a tuple of the appropriate shape, and apply the one to the other. Unlike other languages, this even works at a machine level - a tuple is an ordered set of data on the stack! But the syntax is a nightmare.

We don't really have a good answer for this. We don't know how important it is, so we didn't bother. We wondered about something like this, where we don't use brackets. But it looks hard to parse and scary. It will stay in research for the moment.

```
fn (f: fn(Number, Number) -> Number) -> Number {
    let t = (1, 2)
    //the tuple is applied to the function
    f t
}
```

While we are on this topic, let's talk about the 0-tuple. We don't use `()` to mean a function with no return, we use `Void`, but we try and privilege the `Void` type so it is a true unit type. We obviously did this to make the language more accessible to casual programmers coming from TypeScript, but we actually don't feel too bad about it.

The reason is, we think that it's a lie. At the end of the day, the only function that can return the 0-tuple is one that consumes the 0-tuple. (Yes, a multivariate function can return the empty set, but it does that by discarding its arguments, and so is not really a multivariate function.) Anything else is not a true function in the mathematical sense, so let's be honest and mark the syntax a bit differently. Having said that, we understand the appeal of having  function arguments be a tuple, and the function return be a tuple: by choosing the empty tuple as the arguments yet `Void` as the return type we are being inconsistent between the way we declare inputs and outputs. 

```
fn thisIsClearlyGoingToSideEffect() -> Void
```

But the thing that really sticks in our craw is the 1-tuple. Recall that a function is a map from a set to a set. You only bother to use tuples if your functions are multivariate. Maths can afford to be a bit hazy about this, which is why people don't write `f((x,y))`, but as parser writers we need to be rather more precise.

So this means, *really* our syntax for functions should look like this.

```
//This guy might be pure (it might be a const) or might be a side effect
fn zeroArgFunction () -> Number {...}

//let's call it
let x = zeroArgFunction()

//declare a single arg function
fn singleArgFunction x: Number -> Number {...}

//call it
let y = singleArgFunction 4

//multi args look the same, but are conceptually different -- all functions 
//consume a single argument, and this is syntax for a tuple with named members
fn multiArgFunction(x: Number, x: Number) -> Number {...}

//construct a tuple, apply it to the function
let z = multiArgFunction(5, 6)

//this is a side effecting function. Maybe it logs
fn zeroReturnFunction x: Number -> () {...}

zeroReturnFunction 4
```

This is now completely, brutally honest (although we have a named tuple member syntax problem, but frankly that is for another day) even at the WASM level, and we have no more `Void`. But we think any JavaScript programmer who saw this syntax would run away screaming, so we will probably never do this.


### Lambdas that mutate variables

Everything we have discussed so far has been lambdas tracking *objects*, not *variables*. This is important because we are not supporting a feature that other languages support -- which is the ability to change what a variable points to. We actually could do it if we wanted to, but it's quite scary, so here is a proposal that we won't be implementing, at least for the MVP.

```
{
    let var a = 3
    let f = mut fn() {
        a += 1
    }
    a += 1
    f() //a is now 5
}
```

So how does this work? It translates as this.

```
class heap ptr BoxedNumber{
    constructor(private var inner: Number) {}
}

class heap ptr Packet4 {
    constructor(private a: BoxedNumber) {}

    fn apply() => {
        this.a.inner += 1
    }
}

{
    let a = new mut BoxedNumber(3)
    let f = new mut Packet4(a)
    a.inner += 1
    f.apply(packet)
}
```

Oh no! This is insane! We are boxing the number so we can get at the contents to vary it. This of course means that every reference to the variable `a` has to be changed to `a.inner`. The fact that it's a `heap ptr` is actually not too bad - the variable lasts as long as the scope, as normal; but of course you get to hold onto the object for ages afterwards.

This feels like a very leaky abstraction, so we won't be doing this initially, and lambdas that refer to a `let var` will be a syntax error.

### Holding `heap ptr` classes in other classes

We went through the Option in some detail previously, and discussed how we can't hold `heap ptr`s in `enum`s. 

This is actually a syntax issue, not a conceptual one: we don't have a way to show that the enum is *always* mutable: after all, we can happily have mutable stack classes. What we need is a way of enforcing that the always mutability of the containing class.

So we think we could allow `heap ptr` classes in other classes and enums. When we do this, we will tag the class as being always mutable. This needs some syntax.

```
class stack mut HoldsAnAlwaysMutableThing{
    constructor(public x: mut ThisIsAMutClassOrAHeapPtrClass) {}
}

class heap mut AlsoHoldsAnAlwaysMutableThing{
    constructor(x: mut ThisIsAMutClassOrAHeapPtrClass) {}
}

enum mut AgainHoldsAnAlwaysMutableThing<T>{
    Some{
        constructor(val: mut T) {}
    }
}
```

This means we will have a `MutOption` type, which is not brilliant, but maybe better than nothing?

### Promotion of functions to typeguards

Cast your mind back to our Option enum. We wrote an `isSome` function. That checked the discriminator was `Some` and returned true if it was. Classic options in other languages don't find that very useful, though. You tend to write stuff like this (if for whatever reason you can't use a `map` like you are supposed to).

```
{
    switch(x) {
        x: Option.Some => {
            let v = x.get()
            //do some work on v...
        }
        _ => {}
    }
}
```

The problem with this, is it's super wordy. Really you would like to do this.

```
{
    if (x.isSome()) { 
        v = x.get()
        //do some work on v...
    }
}
```

Of course you can't: the compiler hasn't flagged the function as a type guard, so no narrowing has occurred.

The thing is, it could. If `x` is not a `var`, because the discriminator of an enum is strong const, a function that makes a very simple assessment of an enum discriminator could be promoted to a typeguard. 

This makes the language feel even more scripty, and is therefore good: it requires some research but could be added.

### Being honest about our type system and proofs

We've talked a good game, but we are in over our heads with formal proofs. There are two distinct problems that we see need to overcome. The first is, that we have no idea what we are doing! But we are reading lots of papers.

The second is related, and it's about being honest about our type system. 

We pushed integer range into the compiler. It was complex and specialized, but we got nice small integers as a result. That's great. But we had to open a nasty backdoor into the type system. When we thought about this for, say, strings or arrays, it got really horrible.

The fact is, though, that pushing pre- and post-conditions into the language is really, really compelling. Imagine this piece of hypothetical code.

```
if (x.beginsWith('http://')) {
    //... presumably complain you are not secure...
}
```

The type of x should have started as `String<false>` but in the `if` block, it should be something like `String<beginsWith('http://')>`. For that to have happened, we need two distinct sections to our generic system. The first contains other types used in the construction of that type, either bound or not bound: i.e. what we use generics for today. The second is the section which contains things we have proved. That's the section that describes how big integers are, but properly generalized.

Then we need the compiler to understand that you can make deductions using this. Ideally we want this to be a syntax error.

```
//x is of type String<false>
if (x.beginsWith('http://')) {
    //x is now of type String<beginsWith('http://')>
    //error, if x begins with 'http://', it can never begin with 'https'
    if (x.beginsWith('https')) {

    }
}
```

This is very hard (basically impossible - it means some deep understanding of the string library or a lot of meta-data added in), and by dabbling with this stuff if it is very easy to make the compiler non-terminating. But the fact of being honest means we get a nice slot we can put things into later.

### Run-time type information

Our switch statement has the ability to match against types. All the syntax is there. But, actually, it can't do much, because the only type with run-time type information is the enum.

Which seems wrong, no? So the proposal is that any class that implements an interface gets a uuid. The uuid is created by using a project uuid mingled with a hash of the `<file name>.<type name>`. It will be an entry available from the vtable, will need to be exported with the class definition in the 'quick' pass. This means it's stable but unique.

Then, you can also switch on an interface type to get a derived type. The switch is implemented to get the uuid of the object and a match on the uuids in the code: and it will result in a type guard to narrow the type. Easy easy easy?

No, actually. Generics cause problems here. Let's talk about an interesting but weird case.

```
interface SomeInterface {}

class heap SomeClass<T> implements SomeInterface {
    constructor(x: T) {}
}

fn hello(i: SomeInterface) -> Bool {
    switch(i) {
        x: SomeClass<Number> => x.val > 0
        x: SomeClass<Bool> => x.val
        _ => false
    }
}
```

Bonkers, right? But it's conceptually ok because each instance of `SomeClass` is a different class. But of course, this is really hard to do in the proposed scheme. 

Our very very tentative suggestion is as well as `<file name>.<type name>` we throw either the names or uuids of the types of the generic arguments into the hash too. This is non-trivial because these guys could be templated themselves and in different projects; and we will need some sort of special casing for built ins and constant types. But there is the genesis of a workable scheme here, we think.

### Boxing pushed down to the compiler

Boxing is done at a user level right now. We showed a box class. Let's look at another one.

```
class heap Box<T> {
    constructor(v: T) {}
}

class heap mut MutBox<T> {
    constructor(v: mut T) {}
}
```

That's great and all, and we should have one in the standard library... but we might need boxes done by the compiler. We can't decide. But, while you are pondering that, let's look at the next topic.

### Unions

Union types are great. With all of the above, we think we can do them!

Just quickly, though, let's talk about what a union type actually is.

```
let a: Number | Bool = if(someFun(x)) {
    true
} else {
    4
}
```

This seemingly easy piece of code hides a lot of details. The first is the union type itself.

A union is clearly a thing very much like an enum. It contains a discriminator and some data. The first question is, what is the discriminator? We like the idea of using the uuids we defined for runtime typing, but, actually, that could be a problem: that is a solid 128 bits of data, and clearly the discriminator of the above type needs to be only one bit! Against that, though, it might be useful to have a shared language of unions. Imagine the following code.

```
fn widenAUnion(x: Bool | String) -> Bool | String | Array<Number> {
    x
}
```

This is completely legit. A union should be capable of being arbitrarily widened. If we have a different discriminator set per union, we must map discriminators at run time. If we share a universal discriminator language, that widening is free. So, we will probably not have a discriminator the way an enum does. Instead, we will have a vtable like an interface.

Next, boxing. We think the the union can do that for us, but the fact is the union type size will vary if we have stack allocated classes. We think this is probably OK. It means that unions may get quite big, though.

Casting. A switch on the union type is straightforward to narrow, but a cast into a union type from a non-union is not free - it is a creation of the discriminator plus potentially a huge amount of other data. As a result, we think we will probably only do it explicitly. This will stop bugs in user code, particularly around `if` and `return` statements. So this remains an error, rather than implicitly producing a union.

```
//is a of type 'true | 4'? Nope, it's an error.
let a = if(someFun(x)) {
    true
} else {
    4
}
```

Sanitization. If we have a thing of type `true | false` what is the type of that? It's a `Bool` right? But that is hard. Similarly a thing of type `1 | 2 | 3` is what... an integer? But this requires some sort of conceptual type equivalence. Because our union type is relatively heavyweight, we can't just rebuild our boolean as the union of true and false.

Look this is hard. It took Scala a long time to get it right. We know we probably won't.

### Packing in data - option sizes

### Inlining

### Laziness

### Matching

### Docs

## Things that WASM needs

### A garbage collector

### A good string library

### A good way of shipping runtime

### Access to TZ