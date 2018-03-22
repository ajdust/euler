#!/bin/bash

echo 'Measuring: C++'

cd cpp
make build
/usr/bin/time -v make --always-make runit
make clean

echo 'Measuring: C#'

cd ../csharp
make build
/usr/bin/time -v make --always-make runit
make clean

echo 'Measuring: F#'

cd ../fsharp
make build
/usr/bin/time -v make --always-make runit
make clean

echo 'Measuring: Go'

cd ../go
make build
/usr/bin/time -v make --always-make runit
make clean

echo 'Measuring: Rust'

cd ../rust
make build
/usr/bin/time -v make --always-make runit
make clean

echo 'Measuring: Scala'

cd ../scala
make build
/usr/bin/time -v make --always-make runit
make clean

echo 'Measuring: Java'

cd ../java
make build
/usr/bin/time -v make --always-make runit
make clean