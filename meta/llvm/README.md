# llvm experiments

## Notes


## References

Here are few simple commands.
(also you can reference `build.ninja` rules)

```sh
# build llvm ir file
llc $in.ll -o $out.s

# build llvm ir file to bytecode
llvm-as $in.ll -o $out.bc

# build bytecode file
llc $in.bc -o $out.s

# assemble .s file to object file
as $in.s -o $out.o


# link object file with system runtime library
ld -syslibroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -lSystem $in.o -o $out

# llvm ir interpreters can be used for ll file or bytecode
lli $in.ll

lli $in.bc

# disassembly a bytecode to llvm ir
llvm-dis $in.bc -o $out.ll

# clang emit llvm ir
clang -S $in.c --emit-llvm # llvm ir text format
clang -c $in.c --emit-llvm # llvm ir bytecode


# For any unknown instructions,
# you can also try using `clang -v`
# to see all invocations.
clang -c $in.c -o $out.o -v # compiling

clang $in.o -o $out -v # linking
```
