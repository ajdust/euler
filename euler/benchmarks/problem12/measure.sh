#!/bin/bash

touch results.csv
echo "Language, Exit Code, Elapsed Time (s), Max Resident Set Size (KB)" >> results.csv

cd c
make build
/usr/bin/time -f "      C, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..


cd cpp
make build
/usr/bin/time -f "    C++, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd csharp
make build
/usr/bin/time -f "     C#, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd fsharp
make build
/usr/bin/time -f "     F#, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd go
make build
/usr/bin/time -f "     Go, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd rust
make build
/usr/bin/time -f "   Rust, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd java
make build
/usr/bin/time -f "   Java, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd scala
make build
/usr/bin/time -f "  Scala, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd swift
make build
/usr/bin/time -f "  Swift, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd d
make build
/usr/bin/time -f "      D, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd julia
make build
/usr/bin/time -f "  Julia, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd crystal
make build
/usr/bin/time -f "Crystal, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd nim
make build
/usr/bin/time -f "    Nim, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..

cd python
make build
/usr/bin/time -f " Python, %x, %e, %M" ./main 2>> ../results.csv
make clean
cd ..