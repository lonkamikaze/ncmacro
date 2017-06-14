/** \file
 * Executable parses, executes and prints on stdout.
 *
 * The first file argument is the program to run, provide addititional
 * file names to resolve `M98<filename>` calls.
 *
 * Use -v to include global variable assignments and comments in
 * the output.
 */

#include "M700V.hpp"
#include "IL.hpp"
#include "StreamOutput.hpp"

#include <iostream>
#include <string>

using namespace std::string_literals;

using ncmacro::m700v::FileNames;
using ncmacro::m700v::parse;

using ncmacro::il::Unit;
using ncmacro::il::run;

using ncmacro::stream::Output;
using ncmacro::stream::Sparse;

int main(int argc, char * argv[]) {
	bool verbose{false};
        FileNames files;
        for (int i = 1; i < argc; ++i) {
        	if ("-v"s == argv[i]) {
        		verbose = true;
        	} else {
                	files.push_back(argv[i]);
        	}
        }

        Unit unit;
        parse(unit, files);

        std::cout.precision(10);
        if (verbose) {
        	Output output(std::cout, std::cerr, std::cerr);
        	run(unit, output);
        } else {
        	Sparse output(std::cout, std::cerr, std::cerr);
        	run(unit, output);
        }
        return 0;
}
