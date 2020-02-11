# WA1

WA1 is the working name for a language that compiles down to Web Assembly. It is an attempt at a powerful and fun language. 
Although it has roots in TypeScript, it sheds a lot of the baggage that came from JavaScript, while borrowing concepts from other
languages.

Yes, it needs a fancy name. At some point it will get that.

## Building

To build, first you need Rust. Install cargo. Then, run

```
cargo build
```

in the root. To run, you need to pass the compiler some arguments.

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

### Webpage

One way is to use a webpage that looks something like this

```html
<html>
<body>
  <script>
    fetch('out.wasm').then(response =>
      response.arrayBuffer()
    ).then(bytes =>
      WebAssembly.instantiate(bytes, {imports: {}})
    ).then(results => {
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

and manually call in your browser in the debugger by typing the function name (called `addd` out of the box).

# The language

WA1 is a strongly-typed expression-based language, with its roots in TypeScript syntax. Here we use 'expression-based' to mean that, generally,
the last value of a block is its return value, and you can compose expressions freely.

Because we are not monsters, classic control flow also works, so you can still use `return` if you want.

So, a simple function is written like this. In simple mode, use the export keyword to make it visible outside web assembly.

```
export function mul(x: number, y: number): number {
    x * y;
}
```

But this is also fine:

```
function mul3(x: number, y: number, z: number): number {
    return x * y * z;
}
```

`if` is an expression too!

```
export function negMax(x: number, y: number): number {
    if(x > y)
        -x;
    else {
        -y;
    }
}
```

But it doesn't need to be.

```
export function bang(x: number): number {
    if(x <= 0)
        return 0;

    let out: number = 1.0;

    while(x > 0) {
        out *= x;
        x -= 1;
    }

    out;
}
```

There are some examples in the `tests/simple/one.wa1` file which contains our one working test!

## Features

Currently the following features are supported.

### Types

* number - a 64 bit float.
* bigint - a 64 bit int.
* int - a 32 bit int. Will be auto-casted to number and bigint if needed. This means that an expression like this will actually be an int.
```
let a = 0;
```
* boolean
* Option<T> - very, very limited at the moment. Uses null as the None (and I think the typing might be wrong);

#### Casting

As mentioned above, numeric types will auto-cast. If you need to manually invoke casting, the 'as' keyword is your friend.

```
//totally spurious example
let a = 9 as number;
```

### Control

* Function creation - all arguments and return value must be typed.
* Function calling.
* Local and global variables - type inference works here.
* if, else, while, continue, break, return.

### Linker

If you use full build mode, we generate WASM object files, which can then be linked with a linker. A very simple example exists in the tests folder. 

#### The linker exe

The linker we use is [wasm-ld](https://lld.llvm.org/WebAssembly.html), given that is the only working WASM linker. You will need to install it somehow.
On Ubuntu you can get that by calling 

```
sudo apt install lld-9
```

On Windows and OSX you are on your own, I am afraid. I found building it from source was not impossible. Once you have an exe name (and in in Ubuntu 
it is wasm-ld-9, surprisingly) you need to edit the build-wsb.json file you are building to point to the right location. Yes, I know. This should get better
as either the LLVM WASM toolchain matures, or I get fed up and write a compatible linker.

#### Using it

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
    "wasm_exe": "wasm-ld-9
}
```

The 'entry_point' is the place where we do the module exports. So anything exported in there will be exported to the final wasm.
Anything exported from the 'source_files' will be visible to other files, but not outside the module.

The only working import syntax is 

```
import {*} from "./one"
```

where the '*' means to import everything. Once you import a function 'f' from './m' it will appear as 'm.f'. That means 
to test the example you would run

```
cargo run --bin test-runner -- tests/linker/out/linker.wasm -f 'linker.hello(1, 2);'
```

### Unsafe mode

* `__ptr` type - not yet finished
* `__size_t` type - pretty much the same as a `__ptr`, but makes some bit work a bit type safer
* intrinsics - wrap low level wasm calls
    * `__memorySize()` - `memory.size` instruction
    * `__memoryGrow(0, numPages)` - `memory.grow` instruction
    * `__trap()` - `unreachable` instruction
* `__struct` type - works but you need either the super secret `malloc` function for them to be useful, or cast from a `__ptr` (this is unsafe. The clue
    is in the name...). 
    You do `__struct Hello { a: int; }` to declare, `new Hello {a: 3}` to create 
    (this last uses malloc). A `__struct` is and will always be a raw pointer to memory, used for writing the allocator and other low level things. 

### Under construction 

* Linker - memory next

## TODO

1. Typing 
    1. [x] every expr needs a return type!
    1. [ ] functions decls are exprs, not statements
    1. [ ] Infer return types
    1. [ ] Handle `never` properly - i.e. complain about dead code
    1. [x] do proper auto-widening. means you can widen anything to unknown. Needed for templates, I think. 
        1. [x] means subsuming the number code, which is probably a good thing
    1. [ ] typescript-style const types, where a = 0 means typeof a == 0
    1. [ ] move the bin op types from the ast
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
1. Function calling
    1. [x] simple static function call
    1. [ ] functions as first class objects (but not closures)
1. Known bugs
    1. [ ] fairly sure prefix unary operators are wrong - may need to start at a precedence
    1. [ ] the lexer doesn't parse negative numbers!
    1. [x] import order is not correct - imports need to be first - need a secondary mapping. Claim fixed, needs to be tested
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
    1. [ ] a simple allocator
    1. [ ] a small object allocator
    1. [ ] a gc based on the rajan & bacon paper
        1. [ ] but with gc counter!
1. Structs, ptrs
    1. [x] structs - initial code
    1. [ ] union types are discriminated unions like Rust enums - yeah!
        1. [ ] typescript lets us call members that are in all the types in the unions. This is very convenient, but hard here. Proposal: generate accessor functions for all
            union members, route to the appropriate type, insert that. Not free, but not terrible either.
    1. [ ] option is union T | null
        1. [ ] means map over a union is well understood, I guess
    1. opt: booleans are 1 bit in structs, 32 bits on the heap?
1. Types
    1. [ ] type keyword is a first class type, not an alias (might as well, eh? I always disliked that Scala and TS do that)
1. Closure generation and usage
    1. [ ] closure capture
    1. [ ] call function with closure
    1. [ ] optimization - don't capture consts, you have all the information to roll them inline. This is awesome because I don't think JS can do this
1. templates
    1. [ ] generate meta code to turn into templates, type check against unknown
    1. [ ] we are going to do this at compile time! Yes, thank me later
1. containers
    1. [ ] arrays
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
    1. [ ] A stage 1 parser that will generate a list of file exports - globals, functions, types
    1. [ ] A file format for that
    1. [x] When import commands are run, load that, pull the imports in
1. Linker
    1. [x] object file format that contains
        1. [ ] WASM or an AST
            1. [ ] where the WASM is in PIC format - that is, static memory loads from a func that I can then patch
        1. [ ] A dupe of the export format?
        1. [ ] AST/WASM fragments to be inlined
    1. [x] use wasm-ld
1. tool chain
    1. [ ] npm 
    1. [ ] binaryen optimizer
