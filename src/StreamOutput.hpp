#ifndef _NCMACRO_STREAMOUTPUT_HPP_
#define _NCMACRO_STREAMOUTPUT_HPP_

#include <ostream>

#include "Callback.hpp"

namespace ncmacro {
namespace stream {

class Output : public Callback {
	private:
	std::ostream & out;
	std::ostream & wout;
	std::ostream & eout;

	public:
	Output(std::ostream & out, std::ostream & warn, std::ostream & err) :
	    out{out}, wout{warn}, eout{err} {}

        virtual void block(Block const &) override;
        virtual void assign(addr_t const, value_t const) override;
        virtual void print(string_t const &) override;
        virtual void warn(string_t const &) override;
        virtual void error(string_t const &) override;
};

class Sparse : public Output {
	public:
	using Output::Output;
        virtual void assign(addr_t const, value_t const) override;
        virtual void print(string_t const &) override;
};

} /* namespace stream */
} /* namespace ncmacro */

#endif /* _NCMACRO_STREAMOUTPUT_HPP_ */
