rm tests/simple/out.wasm
cargo run -- build-simple tests/simple/one.wa1 -o=tests/simple/out.wasm
cargo run --bin test-runner -- tests/simple/out.wasm -f 'testSuite();'

cargo run -- build tests/generics-1/build-wsb.json --clean
cargo run --bin test-runner -- tests/generics-1/out/generics_1.wasm -f 'generics_1.test();'

cargo run -- build tests/linker-mem/build-wsb.json --clean
cargo run --bin test-runner -- tests/linker-mem/out/linker_mem.wasm -f 'linker_mem.test();'