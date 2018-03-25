#!/bin/bash

echo -e '\nMeasuring: C++'

cd cpp
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: C#'

cd ../csharp
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: F#'

cd ../fsharp
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: Go'

cd ../go
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: Rust'

cd ../rust
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: Scala'

cd ../scala
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: Java'

cd ../java
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: Crystal'

cd ../crystal
make build
/usr/bin/time -v make --always-make runit
make clean

echo -e '\nMeasuring: Swift'

cd ../swift
make build
/usr/bin/time -v make --always-make runit
make clean