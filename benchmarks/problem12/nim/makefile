
build: main.nim
	nim compile -d:release --opt:speed --o:nim_main main.nim

runit: nim_main
	./nim_main

clean:
	rm ./nim_main
	rm -rf ./nimcache