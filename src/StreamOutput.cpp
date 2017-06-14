#include "StreamOutput.hpp"

using namespace ncmacro::stream;

using ncmacro::Block;
using ncmacro::addr_t;
using ncmacro::value_t;
using ncmacro::string_t;

void Output::block(Block const & words) {
	for (auto const & word : words) {
		this->out << word.name.str << word.value;
	}
	this->out << '\n';
}

void Output::assign(addr_t const var, value_t const value) {
	this->out << "#" << var << " = " << value << '\n';
}

void Output::print(string_t const & str) {
	this->out << str << '\n';
}

void Output::warn(string_t const & str) {
	this->wout << "warning: " << str << '\n';
}

void Output::error(string_t const & str) {
	this->eout << "error: " << str << '\n';
}

void Sparse::assign(addr_t const, value_t const) {}
void Sparse::print(string_t const &) {}
