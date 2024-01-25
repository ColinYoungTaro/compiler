filename=$1
./build/compiler $filename > test.ll
llvm-link test.ll -S ./runtime/runtime.ll -o out.ll
lli out.ll
# rm test.ll