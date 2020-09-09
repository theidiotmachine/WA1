# White paper: a design document for an untitled WebAssembly programing language 

This document defines a proposed WebAssembly language. The WA1 prototype that it comes with implements some of this, and fails to implement others, but generally acts as a research project.

This document is a white paper, because that sounds much better than 'random bag of terrible ideas'.

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

Having armed ourselves with all the above, we can discus class declarations. 

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

The class has a storage declaration between the the `class` keyword and the name. This is one of `stack` or `heap`. We are slightly concerned that the `stack` keyword is longer, because we consider this to be the better option.

Members are declared as constructor arguments. 

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


### Generic classes

In this language, unlike in Java, classes are a compile-time construct. This means a generic class is also a compile-time construct: it does not compile down to an erased class. It is really a template.

Generic classes can only be heap classes. This is because a generic class could be recursive.

```
class heap Generic<T> {
    constructor(a: T, var b: T) {}
}
```

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

We are not completely sure that having the `this` argument be magic is a good idea.

There are no static member functions in this language. You should simply use a normal function. We quite like the Scala `object`, but given we are free from Java's tyranny of everything being an object, we feel we may as well be a bit opinionated. Having said that, we love the way Rust does its constructors, calling them out as static functions, so maybe we will look at that.

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
    var t: T
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

We use the keyword `implements`. This is ugly because of the incredibly similar `implement` used by traits. 

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

We have seen Java code where every class has an interface associated with it, presumably to reduce cognitive load on the callers of the class. Please, we beseech you. Don't do that. C++ does that because it has to. If we think some sort of class api syntax would be useful, that looks like an interface but has no runtime cost, we can do that.

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

## Research notes

These are topics that have been set aside for a later release. Some are true research topics; others are just involve reading up how other languages do them.

### Trait default implementations

For the MVP we don't have trait default implementations. But, most languages do this. We should probably do that.

### Trait members

Scala has trait members. Obviously, a Scala trait is really more like our `interface` but the lessons apply. Basically a regular member is syntax sugar for a `getX()` function, and a variable member also has a `setX()` function; and the code inserts those.

### Trait functions with extra type conditions

Haskell does this and it is rather nice. It basically says that some functions in a type class need extra constraints to be satisfied before they show up. It might simplify some of the iterator stuff - that is, you could have one `IsIterable` trait with multiple `getX` functions, each one of which has a constraint. Maybe I should figure the syntax out now and implement this feature later.

### Type guards and mutability

### Commas are optional

### Generic stack objects

### Type of a constant object with the default constructor

### Mocking at compile time

### Mutating stack objects

### Stack objects implementing interfaces

### Interfaces are a pair of pointers

### Immutable arrays don't need capacity

### Tuples as function arguments; Void as the zero tuple

## Things that WASM needs

### Some way of finding garbage roots on the stack

### A good string library

### A good way of shipping runtime

### Access to TZ