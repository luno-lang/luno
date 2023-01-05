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
The bootstrap compiler is written in Ocaml and requires a UNIX environment to build. If you are on Windows use [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) instead. 

### Requirements:
Provided you have the above you can run the compiler using the following:
```
dune exec luno
```

