#ifndef _NCMACRO_M700V_HPP_
#define _NCMACRO_M700V_HPP_

#include <string>
#include <vector>

namespace ncmacro {
namespace il { struct Unit; }

namespace m700v {
using FileNames = std::vector<std::string>;

void parse(il::Unit &, FileNames const &);

} /* namespace m700v */
} /* namespace ncmacro */

#endif /* _NCMACRO_M700V_HPP_ */
