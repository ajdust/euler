
build: main.d
	ldc2 -O3 main.d -of=d_main && rm d_main.o
	# gdc -O3 -Wall main.d -o d_main

clean:
	rm d_main

runit:
	./d_main