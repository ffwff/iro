# イロハ

**[gitlab](https://gitlab.com/ffwff/iro)** | **[github](https://github.com/ffwff/iro)**

*iroha* is a programming language aiming to be:

  * **safe:** iroha implements various safety mechanisms inspired by Rust, including a borrow checker, automatic resource cleanup and uniqueness types
  * **aesthetic:** iroha's syntax is inspired by Pythonic off-side rule and Ruby's expressive syntax
  * **fast to build:** the compiler is as best optimized as it can be and uses the faster [cranelift](https://github.com/bytecodealliance/wasmtime/tree/master/cranelift) code generator for its debug backend
  * **fast to run:** the compiler can also use the slower C backend for performant release builds (planned)

## Building

iroha currently needs the latest version of rustc (tested on `1.43.0-nightly`) to be installed.

```
git clone https://gitlab.com/ffwff/iro
cargo build
```

## Documentation

(to be added)

## License

MIT License