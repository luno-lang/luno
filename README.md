# Tsuki 🌙
A small toy language with syntax heavily inspired by Lua.

## Dreams
- Sound type system with generics and traits (typeclasses) defined on generics
- A reasonable and simple module syntax

## Building
The bootstrap compiler is written in Ocaml and requires a UNIX environment to build.
There is no planned Windows support so do not ask about it please, if you are on Windows use [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) instead. 

Provided you have the above you can run the compiler using the following:
```sh
dune exec tsuki
```

