CXXFLAGS+=	-std=c++14

all: src/demo src/asm

src/demo: src/demo.o src/M700V.o src/IL.o src/StreamOutput.o
	${CXX} ${CXXFLAGS} -o $@ $>

src/asm: src/asm.o src/M700V.o src/IL.o
	${CXX} ${CXXFLAGS} -o $@ $>

clean:
	rm -f src/*.o src/demo src/asm
