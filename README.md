# WA1

This is the WebAssembly1 project. It needs a fancy name. At some point it will get that.

## Building

To build, first you need Rust. Install cargo. Then, run

```
cargo build
```

in the root. To run, you need to pass the compiler some arguments.

```
cargo run -- wa1/src/one.wa1 -o=out.wasm
```

will parse `wa1/src/one.ws` and generate `out.wasm` in the root.

## Running

To run, you will need some way of running wasm. 

### Runner

We ship a runner built on [wasmtime](https://github.com/bytecodealliance/wasmtime) for easy testing.
To run, call

```
cargo run --bin test-runner -- out.wasm -f 'fourTimes(8);'
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

So, a simple function is written like this. Use the export keyword to make it visible outside web assembly.

```
export function mul(x: number, y: number): number {
    x * y;
}
```

But this is also fine
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

## Features

Currently the following features are supported.

### Types

* number - a 64 bit float.
* bigint - a 64 bit int.
* int - a 32 bit int. Will be autocasted to number and bigint if needed. This means that an expression like this will actually be an int.
```
let a = 0;
```
* boolean

### Control

* Function creation - all arguments and return value must be typed.
* Function calling.
* Local variables - type inference works here.
* if, else, while, continue, break, return.

## TODO

1. [ ] typing 
    1. [x] every expr needs a return type!
    1. [ ] functions decls are exprs, not statements
    1. [ ] Infer return types
    1. [ ] Handle `never` properly
1. [x] expression based language
    1. [x] blocks return a value
    1. [x] no need for a return statement (but still supported - what is this, scala?)
    1. [x] if as an expression 
1. [ ] control flow
    1. [x] if
    1. [x] while
    1. [ ] simple for loops
1. optimise steps. These should be on even on O0
    1. [ ] remove returns right before end
    1. [ ] change tee + drop into set
    1. [ ] remove get + drop
    1. [ ] remove casts straight after consts, and just have the appropriately typed const inline
1. [ ] function calling
    1. [x] simple static function call
    1. [ ] function return type inference
    1. [ ] functions as first class objects (but not closures)
1. [x] assignments and consts
1. bugs
    1. [ ] fairly sure prefix unary operators are wrong - may need to start at a precedence
1. [ ] global variables (currently not init'ed)
1. [ ] a runtime
    1. [ ] a simple allocator
    1. [ ] a small object allocator
    1. [ ] a gc based on the rajan & bacon paper
        1. [ ] but with gc counter!
1. [ ] structs, ptrs
    1. [ ] structs
    1. [ ] union types are discriminated unions like Rust enums - yeah!
    1. [ ] option is union T | null
        1. [ ] means map over a union is well understood, I guess
    1. opt: booleans are 1 bit in structs, 32 bits on the heap?
1. [ ] closure generation and usage
    1. [ ] optimisation - don't capture consts, you have all the information to roll them inline. This is awesome because I don't think JS can do this
1. [ ] containers
    1. [ ] map/foreach are macros
    1. [ ] more complex for loops (in, of)
1. [ ] numbers
    1. [x] there is an i32 type, an i64 type and an 164 type; the compiler auto promotes i32 to the other two types
    1. [ ] colors are a special type
1. imports, exports
  1. imports need to be rolled into the function index map
  1. write a linker (!)
  1. write an export format equiv to ts.d
