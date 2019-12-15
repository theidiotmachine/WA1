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

The syntax of the language is inspired by TypeScript, but it is not completely faithful to it. There is not very much right now.

A function is written like this.

```
function mul(x: number, y: number): number {
    return x * y;
}
```

The export keyword can be used to export the function from webassembly.

```
export function bang(x: number): number {
    let out = 1;

    while(x > 0) {
        out *= x;
        x -= 1;
    }

    return out;
}
```

## Features

Currently the following features are supported.

### Types

* Number - a double. Some support, but not complete
* boolean - mostly there

### Control

* Function creation - all arguments and return value must be typed.
* Function calling.
* Local variables - type inference works here.
* if, else, while, continue, break, return.

## TODO

1. [ ] typing inference
    1. [x] every expr needs a return type!
    1. [ ] functions are exprs, not statements
    1. [ ] Infer return types
1. [ ] control flow
    1. [x] if
    1. [ ] if as an expression 
    1. [x] while
    1. [ ] simple for loops
1. optimise steps
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
1. [ ] options (?)
1. [ ] a runtime
    1. a gc...
    1. ...but don't allocate gc unless you have to! If the function is pure, just delete after
    1. this is surprisingly hard. thoughts
        1. all memory has a gc prev, gc next, prev, size (you don't need next with size) and color (which perhaps might be the top few bits of size)
        1. you can opt out of gc (remove yourself from gclist) and never deleted - you are tartan
        1. you can opt out of gc (remove yourself from gclist) and be pure reference counted - you are beige
        1. the compiler will track lifecycle and decide based on constness, membership of closures, what happens in functions, etc
        1. if it can't figure it out you are full gc
        1. realistically this is what most pointers will be, so this may be a pessimisation
        1. certainly if you are passed into a virtual function, or into a function pointer, we have to gc you because who knows what will happen?
1. [ ] structs, ptrs
    1. opt: booleans are 1 bit in structs, 32 bits on the heap?
1. [ ] closure generation and usage
    1. [ ] optimisation - don't capture consts, you have all the information to roll them inline. This is awesome because I don't think JS can do this
1. [ ] containers
    1. [ ] map/foreach are macros
    1. [ ] more complex for loops (in, of)
1. [ ] numbers
    [x] there is an i32 type, an i64 type and an 164 type; the compiler auto promotes i32 to the other two types
    1. colors are special and magic
1. imports, exports
  1. imports need to be rolled into the function index map
  1. write a linker (!)
  1. write an export format equiv to ts.d
