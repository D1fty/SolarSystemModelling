cxx = g++
cflags = -O3
inc = -I./ -I/opt/local/include/
sfml-libs = -L/opt/local/lib/ -lsfml-window -lsfml-graphics -lsfml-system

all: nbody-compare

nbody-nographics : nbody.cpp
	${cxx} nbody.cpp -o nbody-nographics ${cflags} ${inc}

nbody-compare : nbody.cpp
	${cxx} nbody.cpp -o nbody-compare ${cflags} ${inc} ${sfml-libs} -DGRAPHICS -fopenmp

clean:
	rm nbody-compare nbody-compare