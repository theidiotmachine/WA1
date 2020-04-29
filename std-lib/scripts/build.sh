cargo run -- build std-lib/mem/build-wsb.json --clean
cargo run --bin test-runner -- std-lib/mem/out/mem.wasm -f 'test();'