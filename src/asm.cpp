/** \file
 * Executable parses and prints the generated intermediate code.
 *
 * The first file argument is the program to run, provide addititional
 * file names to resolve `M98<filename>` calls.
 */

#include "M700V.hpp"
#include "IL.hpp"

#include <iostream>

using namespace std::string_literals;

using ncmacro::m700v::FileNames;
using ncmacro::m700v::parse;

using ncmacro::il::Unit;
using ncmacro::il::operator <<;

int main(int argc, char * argv[]) {
        FileNames files;
        for (int i = 1; i < argc; ++i) {
                files.push_back(argv[i]);
        }

        Unit unit;
        parse(unit, files);

	std::cout << unit;
        return 0;
}

