
build: main.scala
	dotc main.scala
	echo "#!/bin/bash" >> main
	echo "dotr facben" >> main
	chmod +x main

clean:
	rm -rf facben*
	rm main
