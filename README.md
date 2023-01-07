# Luno 🌙
A small toy language with syntax heavily inspired by Lua.

## Example
```
fn square(n int) int
  ret n * n
end

square(5) # 5*5 = 25
square(6) # 6*6 = 36
# ...
```

## Building
The bootstrap compiler is written in Rust.
```
cargo run
```

