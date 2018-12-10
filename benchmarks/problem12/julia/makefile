
build: main.jl
	julia package.jl
	echo "#!/bin/bash" >> main
	echo "./builddir/main" >> main
	chmod +x main

clean:
	rm -rf builddir
	rm main
