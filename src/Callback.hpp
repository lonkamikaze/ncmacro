#ifndef _NCMACRO_CALLBACK_HPP_
#define _NCMACRO_CALLBACK_HPP_

#include <vector>
#include <string>

namespace ncmacro {

using addr_t = uint32_t;
using string_t = std::string;
using value_t = double;

struct word_t { char str[4]; };
struct Word {
	word_t name;
	value_t value;
};
using Block = std::vector<Word>;

struct Callback {
        public:
        virtual void block(Block const &) = 0;
        virtual void assign(addr_t const, value_t const) = 0;
        virtual void print(string_t const &) = 0;
        virtual void warn(string_t const &) = 0;
        virtual void error(string_t const &) = 0;
};

} /* namespace ncmacro */

#endif /* _NCMACRO_CALLBACK_HPP_ */
