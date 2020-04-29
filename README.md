# Hello WA1

WA1 is the working name for a language that compiles down to Web Assembly. 

Yes, it needs a fancy name. At some point it will get that.

# A very quick introduction

WA1 is a strongly-typed expression-based language, which borrows from TypeScript, Rust and Scala. Here we use 'expression-based' to mean that, generally,
the last value of a block is its return value, and you can compose expressions freely.

Because we are not monsters, classic control flow also works, so you can still use `return` if you want.

So, a simple function is written like this. In simple build mode, use the export keyword to make it visible outside web assembly.

```
export fn mul(x: Number, y: Number) -> Number {
    x * y
}
```

But this is also fine:

```
fn mul3(x: Number, y: Number, z: Number) -> Number {
    return x * y * z
}
```

`if` is an expression too!

```
export fn negMax(x: Number, y: Number) -> Number {
    if(x > y)
        -x
    else {
        -y
    }
}
```

But it doesn't need to be.

```
export fn bang(x: Number) -> Number {
    if(x <= 0)
        return 0

    let out: Number = 1.0

    while(x > 0) {
        out *= x
        x -= 1
    }

    out
}
```

There are some examples in the `tests/simple/one.wa1` file which contains our original test!

# Using it

To build the compiler, first you need Rust. Install cargo, and make sure you are up to date with `rustup update`. Then, run

```
cargo build
```

in the root. To run the compiler, you need to pass the compiler some arguments.

```
cargo run -- build-simple tests/simple/one.wa1 -o=tests/simple/out.wasm
```

will parse `tests/simple/one.wa1` and generate `tests/simple/out.wasm`.

## Running

To run, you will need some way of running wasm. 

### Runner

We ship a runner built on [wasmtime](https://github.com/bytecodealliance/wasmtime) for easy testing.
To run, call

```
cargo run --bin test-runner -- tests/simple/out.wasm -f 'fourTimes(8);'
```

This will run the init function (always called `__wasm_call_ctors` for compatibility with existing WASM conventions) and then the entry point you give it.

### Webpage

One way is to use a webpage that looks something like this:

```html
<html>
<body>
  <script>
    fetch('out.wasm').then(response =>
      response.arrayBuffer()
    ).then(bytes =>
      WebAssembly.instantiate(bytes, {imports: {}})
    ).then(results => {
      results.instance.exports.__wasm_call_ctors();
      window.addd = results.instance.exports.addd; //or whatever is exported from the file
    });
  </script>
</body>
</html>
```

Serve it with http-server; install that with 

```
npm install -g http-server
```

and manually call in your browser in the debugger by typing the function name (called `addd` out of the box). Note that you have to call 
`__wasm_call_ctors` manually. We don't use start functions, because the wasm linker tools [warn against it](https://github.com/WebAssembly/tool-conventions/blob/master/Linking.md#start-section).


# Another quite quick introduction but less quick than the last one

## Variables

Local and global variables are declared like this.

```
let s = expr
```
The type of `s` is inferred from the expression you assign it.

### A word on semi-colons

You don't need 'em; a return character is equivalent. The things in javascript, like getting weird problems with `return`, are not an issue here. 

## Types

* Number - a 64 bit float.
* Int - An integer. You specify bounds as generic arguments, so `Int<-2147483648, 2147483647>` is a 32 bit signed int. You should rarely need to do that, though; by default 
you get a 32 bit signed, and most things will take that. The compiler tries to do most safe casting for you. This is... a tiny bit crazy and I may not keep it. Ints that stay
within the safe 64 bit float range can be auto-casted to Number.
* Bool - A boolean. 

### Casting

As mentioned above, numeric types will auto-cast. If you need to manually invoke casting, the 'as' keyword is your friend.

```
//totally spurious example
let a = 9 as Number
```

### Member functions on ints

Currently there are four legal int member functions. These are somewhat bodged today.

```
let i = 256
let clz = i.countLeadingZeros()
let ctz = i.countTrailingZeros()
let shl = i.shiftLeft(3)
let shr = i.shiftRight(3)
```

### alias
To create a type alias, use the `alias` keyword. A type alias is what it sounds like, a different name for the same type, not a different type.
```
alias __size_t = Int<0, 4294967295>
```

## Functions

Functions are declared as follows. Use thin arrows for explicit return type declaration, fat arrows will let the type engine figure it out.

```
//explicit return type
fn name(arg: Type) -> Type {
    body
}

//implicit return type
fn name(arg: Type) => {
    body
}
```

The deduction algorithm for return types is very simple. Don't be surprised if you need to help it.

### Generic Functions

Astonishingly, generic functions are supported. Here is the identity function. Not all types can be applied, but that's a work in progress. Stonishingly, expect bugs (but
I am fixing them.)

```
fn id<T>(x: T) -> T { x }
```

To use them you can provide the types, or have it deduce them. 

```
let n1: Number = 4
let n2 = id<Number>(n1)
let n3 = id(n1)
```

The deduction algorithm is very simple, and just matches function argument types against what it's been given. This means that it won't be able to 
deduce a type variable that is only used as a return type.

They are not actually true generics. They specialize for some types -- at the moment, the numeric types -- which means I can avoid a box. It also means
they are sort of a mix between templates and generics. Some days I like saying 'genemplates', other days 'templerics' is more fun.

## Control

### Blocks

The return value of a block is the last expression. So a function that added two numbers would be this:

```
fn add(x: Number, y: Number) => x + y
```

This works because a function expects a block; a single expression or a squiggly bracket enclosed list of expressions is a block; the last expression of a block is what it returns.

### if

`if` is an expression, if it has an else branch.
```
let a = if(b) 3; else 4
```

Type inference will do some work for you if the branches are a bit different, but will complain if they are radically different.

You don't have to have an else branch, mind; you just can't use it as an expression.

```
if(k != 4)
    j = true
```

### while

`while` is not an expression.

```
while(a > 3) {
    a -= 1

    if(a == 100)
        break
}
```

`break` and `continue` work as you would expect.

### return

`return` causes early return of the function you are in.

```
return 4;
```

You don't need it at the end of a function (and is considered bad practice), but it is useful for flow control in complicated functions. If you 
combine fat arrow with return, we try and guess based on all the returns in the function.

## Type guards

### User defined
Inspired by TypeScript, WA1 contains [type guards](https://www.typescriptlang.org/docs/handbook/advanced-types.html). In TypeScript these are used to tell
one JavaScript blob from another. Here, we use them to do limited scope down-casting. What this essentially means is this. Given a bit of code of the form:

```
let x: Bar = whatever()
if(isFoo(x)) {
    x.methodOnFoo()
}
```

any local access of x in the block will automatically cast x to a Foo. This lasts until the block is existed, or x is assigned to. Creating a type guard
function is an unsafe operation, so needs to be done in unsafe mode.

### On integers
Type guards also work on integers. Comparison operators will narrow integer ranges, casting if necessary.

## Linker

Instead of using the simple build mode described above, you may use a full build mode. For this we generate [WASM object files](https://github.com/WebAssembly/tool-conventions/blob/master/Linking.md), which, 
because they are a standard, can then be linked with an external linker. A very simple example exists in the tests folder. 

### The linker exe

The linker we use is [wasm-ld](https://lld.llvm.org/WebAssembly.html), given that is the only working WASM linker. You will need to install it somehow.
On Ubuntu you can get that by calling 

```
sudo apt install lld
```

On Windows probably the easiest is to install a pre-built binary from [the releases page, here](https://releases.llvm.org/download.html). MacOS you are on your own, I am afraid. 
I found building it from source was not impossible, but I imagine you may be able to install from the above link. Once you have an exe name (and in in Ubuntu 
it is `wasm-ld-9`, surprisingly) you need to edit the `build-wsb.json `file you are building to point to the right location. Yes, I know. This should get better
as either the LLVM WASM toolchain matures, or I finally build a configuration tool.

### Using it

When you are set up, call this to run the sample.

```
cargo run -- build tests/linker/build-wsb.json --clean
```

Here, the 'clean' arg will always force a build. You can omit if you wish. The build-wsb.json format is subject to change. However, at the moment,
here it is.

```json
{
    "entry_point": {
        "file_name": "two.wa1",
        "is_unsafe": false
    },
    "source_files": [
        {
            "file_name": "one.wa1",
            "is_unsafe": false
        }
    ],
    "src_path": "./src",
    "out_path": "./out",
    "module_name": "linker",
    "wasm_exe": "wasm-ld-9'
}
```

The `entry_point` is the place where we do the module exports. So anything exported in there will be exported to the final wasm.
Anything exported from the `source_files` will be visible to other files, but not outside the module.

The only working import syntax is 

```
import {*} from "./one"
```

where the `*` means to import everything. Once you import a function `f` from `./m` it will appear as `m.f`. That means 
to test the example you would run

```
cargo run --bin test-runner -- tests/linker/out/linker.wasm -f 'linker.hello(1, 2);'
```

## Unsafe mode

Unsafe mode is designed to be a mode that library writers can write low-level code. It feels like C in that it is 
not much more than structured WASM. In order to use it you need to somehow pass the unsafe arg to the compiler. Syntax is deliberately scary.

A leading `__` is pronounced 'unsafe', by the way.

* `__Ptr` type. Pointer to raw memory.
* intrinsics - wrap low level wasm calls
    * `__memorySize()` - `memory.size` instruction
    * `__memoryGrow(0, numPages)` - `memory.grow` instruction
    * `__trap()` - `unreachable` instruction
* `__Option` - is a essentially a nullable pointer. You can have it be a `__Null` or `__Some<T>`. It will end up being the core of the 
    real option. It's kind of horrible because it's pretending to be a union but it really is a nullable pointer.
* `__struct` type - works but you need either the super secret `malloc` function for them to be useful 
    (which doesn't exist, so good luck with that), cast from a `__Ptr` (this is unsafe. The clue
    is in the name...), or use `__static` (which does actually work and is a C-style static allocation). 
    You do `__struct Hello { a: Int; }` to declare, `new Hello {a: 3}` to dynamically allocate (which uses malloc), 
    or `__static Hello {a: 3}`. A `__struct` is and will always be a raw pointer to memory, used for writing the allocator and other low level things. 
* `__typeguard` This keyword lets you define type guards. Here is an example.
```
export fn __Option_isNull<T: __struct T>(x: __Option<T>) -> Bool __typeguard {
    true => __Null
    false => __Some<T>
} {
    x == __Null
}
```


# TODO

1. Typing 
    1. [x] every expr needs a return type!
    1. [ ] functions decls are exprs, not statements
    1. [x] Infer return types
    1. [ ] Handle `never` properly - i.e. complain about dead code
    1. [x] do proper auto-widening. means you can widen anything to unknown. Needed for templates, I think. 
        1. [x] means subsuming the number code, which is probably a good thing
    1. [x] typescript-style const types, where a = 0 means typeof a == 0
    1. [x] move the bin op types from the ast
    1. [ ] clean up the cast code
    1. [ ] bool literals
    1. [ ] check __Option equality
1. Expression based language
    1. [x] blocks return a value
    1. [x] no need for a return statement (but still supported - what is this, scala?)
    1. [x] if as an expression 
1. Error handling
    1. [x] all exprs have a loc
    1. [ ] all errors have a loc
1. Control flow
    1. [x] if
    1. [x] while
    1. [ ] simple for loops
    1. [x] assignments and consts
    1. [x] global variables
    1. [ ] match statements 
        1. [ ] implement [this](http://cmph.sourceforge.net/papers/esa09.pdf) for strings
    1. [ ] assign is void
1. Strings
    1. [ ] a 'char' is a unicode grapheme cluster - see [this](https://manishearth.github.io/blog/2017/01/14/stop-ascribing-meaning-to-unicode-code-points/) - this guy has some cool rust libs
1. Optimise steps. These should be on even on O0
    1. [x] use the 'consume' code to not write drops
    1. [ ] remove returns right before end
    1. [x] change tee + drop into set
    1. [x] remove get + drop
    1. [ ] remove casts straight after consts, and just have the appropriately typed const inline
    1. [ ] == 0 is eqz
    1. [ ] globals that are init to a const should use the wasm global init mechanism
    1. [ ] when calling __static, copy true data into the data section and drop the initializer expression
    1. [ ] tail recursion
1. Function calling
    1. [x] simple static function call
    1. [ ] functions as first class objects (but not closures)
    1. [ ] allow tuples to be used as function args (so e.g. instead of `f(1, 2)`, we also allow `let t = (1, 2); f t`)
1. Known bugs
    1. [ ] fairly sure prefix unary operators are wrong - may need to start at a precedence
    1. [ ] the lexer doesn't parse negative numbers!
    1. [x] import order is not correct - imports need to be first - need a secondary mapping. Claim fixed, needs to be tested
    1. [ ] type guards - in unguarded loops
1. Inlining
    1. [ ] inline numeric constants?
    1. [ ] or full blown inlining, of which this is just a special case?
        1. initially write a function that inlines code that contains no `return`s as an ast operation
        1. copy any non-variable exprs into local variables
1. Purity
    1. [ ] Record (some? one?) of states pre expr
        * globally pure (no non-const global access)
        * locally pure (true purity)
        * locally impure, globally pure (essentially reassignment to a local variable - referential integrity but not proper FP)
        * changing passed in object
        * side effecting (e.g. writing a mutable global, writing a file)
        * non-deterministic (e.g. currenttime, memorysize, reading a global or file)
    1. [ ] evaluate globally and locally pure functions if they have literals passed in. Is this... insane?
1. A runtime
    1. [x] a simple allocator
    1. [x] a small object allocator
    1. [ ] a gc based on the rajan & bacon paper
        1. [ ] but with assignment counter
1. Structs, ptrs
    1. [x] structs - initial code
    1. [ ] union types are discriminated unions like Rust enums - yeah!
        1. [ ] typescript lets us call members that are in all the types in the unions. This is very convenient, but hard here. Proposal: generate accessor functions for all
            union members, route to the appropriate type, insert that. Not free, but not terrible either.
    1. [ ] option is union T | null... maybe?
        1. [ ] means map over a union is well understood, I guess
    1. [ ] make small ints fit into small spaces
    1. opt: booleans are 1 bit in structs, 32 bits on the heap?
1. Types
    1. [ ] tuples - are just collections of locals, so pass by value (needs wasm multi value for some things)
    1. [ ] tuples - need a syntax for tuple 1. Probably `Tuple<int>(3)`
    1. [ ] type keyword is a first class type, not an alias (might as well, eh? I always disliked that Scala and TS do that)
1. Closure generation and usage
    1. [ ] closure capture
    1. [ ] call function with closure
    1. [ ] optimization - don't capture consts, you have all the information to roll them inline. This is awesome because I don't think JS can do this
1. templerics
    1. [x] for funcs generate meta code to turn into templates
    1. [ ] for types generate meta types
    1. [x] implicit type args on instantiation
    1. [x] support fat arrow
    1. [x] partial instantiation - specifically `fn f1<T>(x: T) => {} fn f2<U>(x: U) => f1<U>(x)` currently generates an UnresolvedTypeArg error
1. containers
    1. [ ] arrays
        1. [ ] pass by value. increment rc on creation, if function contains a modification, not otherwise
        1. [ ] 'ref' keyword for pass by ref
        1. [ ] if a function takes an object and modifies it and returns that object, mark it. When you do let a = f(a) you should be able to remove ref count incs
        1. [ ] a function that takes an object by ref and modifies it is marked. it will not inc the ref count; instead, the calling function does.
    1. [ ] hashmaps (probably called 'objects' to be JS friendly)
    1. [ ] map/foreach are macros
    1. [ ] more complex for loops (in, of)
1. Numbers
    1. [x] there is an i32 type, an i64 type and an 164 type; the compiler auto promotes i32 to the other two types
    1. [ ] colors are a special type
1. Imports and exports
    1. syntax
        1. [ ] import {*} as x from y => means will get everything from y as x.a, x.b
        1. [ ] import {a} as x from y => means will get x.a 
        1. [x] import {*} from y => means will get y.a, y.b, etc
        1. [ ] import {a} from y => means will get y.a
    1. [x] A stage 1 parser that will generate a list of file exports - globals, functions, types
    1. [x] A file format for that
    1. [x] When import commands are run, load that, pull the imports in
    1. [ ] Functions to be inlined
1. Linker
    1. [x] object file format that contains
    1. [x] use wasm-ld
    1. [x] `fatal-warnings`
1. tool chain
    1. [ ] npm 
    1. [ ] binaryen optimizer
