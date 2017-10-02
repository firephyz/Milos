all: link
	@echo "Building MILVA..."

link: compile
	gcc -o bin/milva build/milva.o

compile: init
	gcc -c -o build/milva.o src/milva.c

init:
	mkdir -p build
	mkdir -p bin

clean:
	rm -rf build
	rm -rf bin
