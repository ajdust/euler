cd cpp
echo 'cd cpp'
clang++ -O3 -Wall -pedantic -Wextra -std=c++17 main.cpp -o main.exe -I"C:\Users\aaron\Downloads\boost_1_55_0\boost_1_55_0\boost"
hyperfine -m 5 $pwd/main.exe
cd ..

cd csharp
echo 'cd csharp'
dotnet publish -c Release -r win-x64
hyperfine -m 5 $pwd/bin/Release/net5.0/win-x64/publish/factorbench.exe
cd ..

cd fsharp
echo 'cd fsharp'
dotnet publish -c Release -r win-x64
hyperfine -m 5 $pwd/bin/Release/net5.0/win-x64/publish/factorbench.exe
cd ..

cd rust
echo 'cd rust'
rustc -C opt-level=3 main.rs -o main.exe
hyperfine -m 5 $pwd/main.exe
cd ..

cd nim
echo 'cd nim'
nim "compile" -d:release --opt:speed --o:main.exe main.nim
hyperfine -m 5 $pwd/main.exe
cd ..

cd go
echo 'cd go'
go build main.go
hyperfine -m 5 $pwd/main.exe
cd ..

cd java
echo 'cd java'
javac ./main.java
hyperfine -m 5 "java Main.class"
cd ..

cd python
echo 'cd python'
hyperfine -m 5 "python ./main.py"
cd ..

cd javascript
echo 'cd javascript'
hyperfine -m 5 "node main.js"
cd ..

cd swift
echo 'cd swift'
swiftc .\main.swift -o main.exe -sdk $env:SDKROOT -I $env:SDKROOT/usr/lib/swift -L $env:SDKROOT/usr/lib/swift/windows
hyperfine -m 5 main.exe
cd ..