# WA1

This is the WebAssembly1 project. It needs a fancy name. At some point it will get that.

## Building

To build, first you need Rust. Install cargo. Then, run

> cargo build

in the root. At the moment, 

> cargo run

will parse `main/src/one.ws` and generate `out.wasm` in the root. This will produce a function `addd` that returns the max of two numbers.

## Running

To run, you will need some way of running wasm. The easiest is to use a webpage that looks like this

```html
<html>
<body>
  <script>
    fetch('out.wasm').then(response =>
      response.arrayBuffer()
    ).then(bytes =>
      WebAssembly.instantiate(bytes, {imports: {}})
    ).then(results => {
      window.addd = results.instance.exports.addd;
    });
  </script>
</body>
</html>
```

Serve it with http-server; install that with 

> npm install -g http-server

and manually call in your browser in the debugger by typing the function name (called `addd` out of the box).

## Features

Currently the following features are supported

### Types

* Number - a double. Some support, but not complete
* boolean - mostly there

### Language features

* Function creation - all arguments and return value must be typed
* Local variables - type inference works here. Note that you cannot currently reassign to locals
* if, else, return.

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
1. [ ] function calling
    1. [ ] function return type inference
    1. [ ] functions as first class objects (but not closures)
1. [x] assignments and consts
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
    1. option 1 - everything is an f64 except for indexes which are i32 and the compiler figures it out
    1. option 2 - everything is a fixed width 64 bit number excecpt for other things
    1. colors are special and magic
1. imports, exports
  1. imports need to be rolled into the function index map
  1. write a linker (!)
  1. write an export format equiv to ts.d
