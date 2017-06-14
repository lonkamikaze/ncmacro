#include "IL.hpp"

#include <unordered_map>
#include <stack>
#include <cmath>
#include <cassert>
#include <algorithm>
#include <sstream>
#include <cstdio>
#include <climits>

using namespace ncmacro::il;

namespace {

using ncmacro::addr_t;
using ncmacro::word_t;
using ncmacro::value_t;
using ncmacro::string_t;
using ncmacro::Word;
using ncmacro::Callback;
using ncmacro::Block;

struct Value {
	bool initialised{false};
	value_t value{0};

	Value() = default;
	Value(value_t const value) : initialised{true}, value{value} {}

	operator value_t() const {
		return value;
	}
	string_t str() const {
		std::ostringstream stream;
		if (this->initialised) {
			stream << this->value;
		} else {
			stream << "<undefined>";
		}
		return stream.str();
	}
};

using Ram = std::unordered_map<addr_t, Value>;
using Stack = std::vector<Value>;
using Local = std::stack<Ram>;

template <class Container, typename T>
void push(Container & stack, T const & value) {
	stack.push_back(value);
}

template <class Container>
auto pop(Container & stack) {
	auto result = stack.back();
	stack.pop_back();
	return result;
}

template <class Container>
auto pop(Container & stack, addr_t const off) {
	auto iter = std::end(stack) - 1 - off;
	auto result = *iter;
	stack.erase(iter);
	return result;
}

template <class Container>
void clone(Container & stack, addr_t const count) {
	auto end = std::end(stack);
	stack.insert(end, end - count, end);
}

bool operator ==(Word const & lhs, Word const & rhs) {
	return lhs.value == rhs.value &&
	string_t{lhs.name.str} == rhs.name.str;
}

struct Memory {
	Stack stack;
	Ram ram;
};


constexpr size_t sum() { return 0; }

template <typename... Tail>
constexpr size_t sum(size_t const head, Tail const... tail) {
	return head + sum(tail...);
}

template <typename... Ts> constexpr size_t size() {
	return sum(sizeof(Ts)...);
}

constexpr addr_t wordcount(OpCode const op) {
	switch (op) {
	case OpCode::LOAD:
		return size<OpCode, value_t>();
	case OpCode::GLOAD:
	case OpCode::LLOAD:
	case OpCode::CLONE:
	case OpCode::GSTORE:
	case OpCode::LSTORE:
	case OpCode::POP:
	case OpCode::FLIP:
	case OpCode::CALL:
	case OpCode::LCALL:
	case OpCode::GOTO:
	case OpCode::LBL:
	case OpCode::CALLLBL:
	case OpCode::WRAPLBL:
	case OpCode::LOADSTR:
	case OpCode::PRINT:
	case OpCode::WARN:
	case OpCode::ERROR:
		return size<OpCode, addr_t>();
	case OpCode::BAPPEND:
	case OpCode::BHAS:
		return size<OpCode, word_t>();
	case OpCode::BMATCH:
		return size<OpCode, word_t, value_t>();
	case OpCode::BLOAD:
		return size<OpCode, word_t, addr_t>();
	default:
		return size<OpCode>();
	}
}

template <typename T, size_t Size>
constexpr size_t countof(T const (&)[Size]) { return Size; }

constexpr char const * const opfmts[]{
	"%6d  NOP\n",
	"%6d  LOAD    %f\n",
	"%6d  LOAD    0\n",
	"%6d  LOAD    1\n",
	"%6d  GLOAD   #%d\n",
	"%6d  LLOAD   #%d\n",
	"%6d  CLONE   %d\n",
	"%6d  GSTORE  #%d\n",
	"%6d  LSTORE  #%d\n",
	"%6d  POP     %d\n",
	"%6d  FLIP    %d\n",
	"%6d  CALL    @%d\n",
	"%6d  RET\n",
	"%6d  LCALL   @%d\n",
	"%6d  LRET\n",
	"%6d  GOTO    @%d\n",
	"%6d  EXIT\n",
	"%6d  LBL     %d\n",
	"%6d  CALLLBL @%d\n",
	"%6d  GOTOLBL\n",
	"%6d  WRAPLBL @%d\n",
	"%6d  IF\n",
	"%6d  MUL\n",
	"%6d  DIV\n",
	"%6d  MOD\n",
	"%6d  AND\n",
	"%6d  ADD\n",
	"%6d  SUB\n",
	"%6d  OR\n",
	"%6d  XOR\n",
	"%6d  EQ\n",
	"%6d  NE\n",
	"%6d  GT\n",
	"%6d  LT\n",
	"%6d  GE\n",
	"%6d  LE\n",
	"%6d  EQI\n",
	"%6d  NEI\n",
	"%6d  NEG\n",
	"%6d  NOT\n",
	"%6d  FROMDEG\n",
	"%6d  TODEG\n",
	"%6d  SIN\n",
	"%6d  COS\n",
	"%6d  TAN\n",
	"%6d  ASIN\n",
	"%6d  ATAN\n",
	"%6d  ACOS\n",
	"%6d  SQRT\n",
	"%6d  ABS\n",
	"%6d  FROMBCD\n",
	"%6d  TOBCD\n",
	"%6d  ROUND\n",
	"%6d  FLOOR\n",
	"%6d  TRUNC\n",
	"%6d  CEIL\n",
	"%6d  PAD\n",
	"%6d  LN\n",
	"%6d  EXP\n",
	"%6d  BCOMMIT\n",
	"%6d  BFLUSH\n",
	"%6d  BAPPEND %.3s\n",
	"%6d  BMATCH  %.3s, %f\n",
	"%6d  BHAS    %.3s\n",
	"%6d  BLOAD   %.3s, %d\n",
	"%6d  LOADSTR \"%.40s\"\n",
	"%6d  PRINT   \"%.40s\"\n",
	"%6d  WARN    \"%.40s\"\n",
	"%6d  ERROR   \"%.40s\"\n",
};

static_assert(countof(opfmts) == static_cast<size_t>(OpCode::VERSION),
              "every opcode must be printable");

constexpr char const * opfmt(OpCode const op) {
	return opfmts[static_cast<size_t>(op)];
}

constexpr double const PI{3.14159265358979323846};

value_t bcd_to_bin(value_t const value) {
	addr_t const bcd{static_cast<addr_t>(abs(value))};
	value_t bin{0};
	for (int i = (sizeof(bcd) * CHAR_BIT) - 4; i >= 0; i -= 4) {
		bin = bin * 10 + ((bcd >> i) & 0xf);
	}
	return bin;
}

value_t bin_to_bcd(value_t const value) {
	addr_t bin{static_cast<addr_t>(abs(value))};
	value_t bcd{0};
	for (int i = 0; bin; bin /= 10, i += 4) {
		bcd = (bin % 10) << i;
	}
	return bcd;
}

template <typename T>
T get(Code::const_iterator const ptr) {
	T result;
	for (size_t i = 0; i < sizeof(T); ++i) {
		reinterpret_cast<char *>(&result)[i] = ptr[i];
	}
	return result;
}

template <size_t Size, typename... ArgTs>
auto sprintf_safe(char (&dst)[Size], ArgTs const... args) {
	return snprintf(dst, Size, args...);
}

/**
 * @param out
 *	The output stream
 * @param unit
 *	The translation unit
 * @param begin,end
 *	Iterator range to print (excluding end)
 */
void printcode(std::ostream & out, Unit const & unit,
               Code::const_iterator begin, Code::const_iterator const end) {
	Code::const_iterator const first = std::begin(unit.code);
	char buf[80];
	for (; begin != end; begin += wordcount(get<OpCode>(begin))) {
		auto const i = begin - first;
		auto const argp = begin + sizeof(OpCode);
		switch (get<OpCode>(begin)) {
		case OpCode::LOAD:
			sprintf_safe(buf, opfmt(get<OpCode>(begin)), i,
			             get<value_t>(argp));
			break;
		case OpCode::CALL:
		case OpCode::LCALL:
			sprintf_safe(buf, opfmt(get<OpCode>(begin)), i,
			             get<addr_t>(argp),
			             get<addr_t>(argp + sizeof(addr_t)));
			break;
		case OpCode::GLOAD:
		case OpCode::LLOAD:
		case OpCode::CLONE:
		case OpCode::GSTORE:
		case OpCode::LSTORE:
		case OpCode::POP:
		case OpCode::FLIP:
		case OpCode::GOTO:
		case OpCode::LBL:
		case OpCode::CALLLBL:
		case OpCode::WRAPLBL:
			sprintf_safe(buf, opfmt(get<OpCode>(begin)), i,
			             get<addr_t>(argp));
			break;
		case OpCode::BAPPEND:
		case OpCode::BHAS:
			sprintf_safe(buf, opfmt(get<OpCode>(begin)), i,
			             get<word_t>(argp).str);
			break;
		case OpCode::BMATCH:
			sprintf_safe(buf, opfmt(get<OpCode>(begin)), i,
			             get<word_t>(argp).str,
			             get<value_t>(argp + sizeof(word_t)));
			break;
		case OpCode::BLOAD:
			sprintf_safe(buf, opfmt(get<OpCode>(begin)), i,
			             get<word_t>(argp).str,
			             get<addr_t>(argp + sizeof(word_t)));
			break;
		case OpCode::LOADSTR:
		case OpCode::PRINT:
		case OpCode::WARN:
		case OpCode::ERROR:
			{
				auto const & str =
				    unit.strings[get<addr_t>(argp)];
				sprintf_safe(buf, opfmt(get<OpCode>(begin)), i,
				             str.c_str());
			}
			break;
		default:
			sprintf_safe(buf, opfmt(get<OpCode>(begin)), i);
			break;
		}
		out << buf;
	}
}

void run(Unit const & unit, Callback & callback, Memory & global) {
	Local local;
	Block words;
	addr_t pc = 0;

	auto const begin = std::begin(unit.code);

	/* initialise local ram */
	local.push(Ram{});

	while (pc < unit.code.size()) {
		auto const argp = begin + pc + sizeof(OpCode);
		switch (get<OpCode>(begin + pc)) {
		case OpCode::NOP:
			break;
		case OpCode::LOAD:
			push(global.stack, get<value_t>(argp));
			break;
		case OpCode::LOAD0:
			push(global.stack, value_t{0});
			break;
		case OpCode::LOAD1:
			push(global.stack, value_t{1});
			break;
		case OpCode::GLOAD:
			push(global.stack, global.ram[get<addr_t>(argp)]);
			break;
		case OpCode::LLOAD:
			push(global.stack, local.top()[get<addr_t>(argp)]);
			break;
		case OpCode::CLONE:
			clone(global.stack, get<addr_t>(argp));
			break;
		case OpCode::GSTORE:
			global.ram[get<addr_t>(argp)] = global.stack.back();
			callback.assign(get<addr_t>(argp), global.stack.back());
			global.stack.pop_back();
			break;
		case OpCode::LSTORE:
			local.top()[get<addr_t>(argp)] = pop(global.stack);
			break;
		case OpCode::POP:
			{
				auto arg = get<addr_t>(argp);
				global.stack.erase(std::end(global.stack) - arg,
				                   std::end(global.stack));
			}
			break;
		case OpCode::FLIP:
			{
				auto arg = get<addr_t>(argp);
				std::reverse(std::end(global.stack) - arg,
				             std::end(global.stack));
			}
			break;
		case OpCode::CALL:
			push(global.stack, pc);
			pc = get<addr_t>(argp);
			local.push(Ram{});
			continue;
		case OpCode::RET:
			local.pop();
			pc = pop(global.stack);
			break;
		case OpCode::LCALL:
			push(global.stack, pc);
			pc = get<addr_t>(argp);
			continue;
		case OpCode::LRET:
			pc = pop(global.stack);
			break;
		case OpCode::GOTO:
			pc = get<addr_t>(argp);
			continue;
		case OpCode::EXIT:
			return;
		case OpCode::LBL:
			break;
		case OpCode::CALLLBL:
			{
				/* backup lbl */
				auto lbl = pop(global.stack);
				push(global.stack, pc);
				pc = get<addr_t>(argp);
				local.push(Ram{});
				/* put label back on top of the stack */
				push(global.stack, lbl);
			}
			/* fallthrough */
		case OpCode::GOTOLBL:
			for (auto addr = pc + wordcount(get<OpCode>(begin + pc));
			     addr != pc;) {
				if (get<OpCode>(begin + addr) == OpCode::LBL &&
				    get<addr_t>(begin + addr + sizeof(OpCode))
				    == global.stack.back()) {
					pc = addr;
					break;
				}
				addr += wordcount(get<OpCode>(begin + addr));
				if (addr >= unit.code.size()) {
					addr = 0;
				} else if (get<OpCode>(begin + addr) ==
				           OpCode::WRAPLBL) {
					addr = get<addr_t>(begin + addr +
					                   sizeof(OpCode));
				}
			}
			global.stack.pop_back();
			break;
		case OpCode::WRAPLBL:
			return;
		case OpCode::IF:
			if (pop(global.stack) == 0.) {
				pc += wordcount(OpCode::IF);
			}
			break;
		case OpCode::MUL:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs * rhs;
			}
			break;
		case OpCode::DIV:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs / rhs;
			}
			break;
		case OpCode::MOD:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = fmod(lhs, rhs);
			}
			break;
		case OpCode::AND:
			{
				addr_t rhs = pop(global.stack);
				addr_t lhs = global.stack.back();
				global.stack.back() = lhs & rhs;
			}
			break;
		case OpCode::ADD:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs + rhs;
			}
			break;
		case OpCode::SUB:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs - rhs;
			}
			break;
		case OpCode::OR:
			{
				addr_t rhs = pop(global.stack);
				addr_t lhs = global.stack.back();
				global.stack.back() = lhs | rhs;
			}
			break;
		case OpCode::XOR:
			{
				addr_t rhs = pop(global.stack);
				addr_t lhs = global.stack.back();
				global.stack.back() = lhs ^ rhs;
			}
			break;
		case OpCode::EQ:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs == rhs ? 1. : 0.;
			}
			break;
		case OpCode::NE:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs != rhs ? 1. : 0.;
			}
			break;
		case OpCode::GT:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs > rhs ? 1. : 0.;
			}
			break;
		case OpCode::LT:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs < rhs ? 1. : 0.;
			}
			break;
		case OpCode::GE:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs >= rhs ? 1. : 0.;
			}
			break;
		case OpCode::LE:
			{
				value_t rhs = pop(global.stack);
				value_t lhs = global.stack.back();
				global.stack.back() = lhs <= rhs ? 1. : 0.;
			}
			break;
		case OpCode::EQI:
			{
				auto rhs = pop(global.stack);
				auto lhs = global.stack.back();
				global.stack.back() =
				    lhs.initialised == rhs.initialised
				    ? 1. : 0.;
			}
			break;
		case OpCode::NEI:
			{
				auto rhs = pop(global.stack);
				auto lhs = global.stack.back();
				global.stack.back() =
				    lhs.initialised != rhs.initialised
				    ? 1. : 0.;
			}
			break;
		case OpCode::NEG:
			{
				value_t rhs = global.stack.back();
				global.stack.back() = -rhs;
			}
			break;
		case OpCode::NOT:
			{
				value_t rhs = global.stack.back();
				global.stack.back() = rhs == 0. ? 1. : 0.;
			}
			break;
		case OpCode::FROMDEG:
			global.stack.back() = global.stack.back() / 180. * PI;
			break;
		case OpCode::TODEG:
			global.stack.back() = global.stack.back() * 180. / PI;
			break;
		case OpCode::SIN:
			global.stack.back() = sin(global.stack.back());
			break;
		case OpCode::COS:
			global.stack.back() = cos(global.stack.back());
			break;
		case OpCode::TAN:
			global.stack.back() = tan(global.stack.back());
			break;
		case OpCode::ASIN:
			global.stack.back() = asin(global.stack.back());
			break;
		case OpCode::ATAN:
			global.stack.back() = atan(global.stack.back());
			break;
		case OpCode::ACOS:
			global.stack.back() = acos(global.stack.back());
			break;
		case OpCode::SQRT:
			global.stack.back() = sqrt(global.stack.back());
			break;
		case OpCode::ABS:
			global.stack.back() = abs(global.stack.back());
			break;
		case OpCode::FROMBCD:
			global.stack.back() = bcd_to_bin(global.stack.back());
			break;
		case OpCode::TOBCD:
			global.stack.back() = bin_to_bcd(global.stack.back());
			break;
		case OpCode::ROUND:
			global.stack.back() = round(global.stack.back());
			break;
		case OpCode::FLOOR:
			global.stack.back() = floor(global.stack.back());
			break;
		case OpCode::TRUNC:
			global.stack.back() = trunc(global.stack.back());
			break;
		case OpCode::CEIL:
			global.stack.back() = ceil(global.stack.back());
			break;
		case OpCode::PAD:
			{
				value_t value = global.stack.back();
				global.stack.back() =
				    value >= 0. ? ceil(value) : floor(value);
			}
			break;
		case OpCode::LN:
			global.stack.back() = log(global.stack.back());
			break;
		case OpCode::EXP:
			global.stack.back() = exp(global.stack.back());
			break;
		case OpCode::BCOMMIT:
			callback.block(words);
			break;
		case OpCode::BFLUSH:
			words.clear();
			break;
		case OpCode::BAPPEND:
			push(words, Word{get<word_t>(argp), pop(global.stack)});
			break;
		case OpCode::BMATCH:
			{
				value_t count = 0;
				Word const match{
					get<word_t>(argp),
					get<value_t>(argp + sizeof(word_t))
				};
				for (auto const & word : words) {
					count += (match == word);
				}
				push(global.stack, count);
			}
			break;
		case OpCode::BHAS:
			{
				value_t count = 0;
				auto const match =
				    string_t{get<word_t>(argp).str};
				for (auto const & word : words) {
					count += (match == word.name.str);
				}
				push(global.stack, count);
			}
			break;
		case OpCode::BLOAD:
			{
				auto const match =
				    string_t{get<word_t>(argp).str};
				auto count = get<addr_t>(argp + sizeof(word_t));
				value_t value = 0;
				for (auto const & word : words) {
					if (match == word.name.str &&
					    count-- == 0) {
						value = word.value;
						break;
					}
				}
				push(global.stack, value);
			}
			break;
		case OpCode::LOADSTR:
			push(global.stack, get<addr_t>(argp));
			break;
		case OpCode::PRINT:
			callback.print(unit.strings[get<addr_t>(argp)]);
			break;
		case OpCode::WARN:
			callback.warn(unit.strings[get<addr_t>(argp)]);
			return;
		case OpCode::ERROR:
			callback.error(unit.strings[get<addr_t>(argp)]);
			return;
		default:
			assert(false && "invalid opcode");
			return;
		}
		pc += wordcount(get<OpCode>(begin + pc));
	}
}

} /* namespace */

template void ncmacro::il::append<OpCode>(Unit &, OpCode const);
template void ncmacro::il::append<addr_t>(Unit &, addr_t const);
template void ncmacro::il::append<value_t>(Unit &, value_t const);
template void ncmacro::il::append<word_t>(Unit &, word_t const);

void ncmacro::il::append(Unit & unit, string_t const & str) {
	addr_t addr;
	for (addr = 0; addr < unit.strings.size(); ++addr) {
		if (str == unit.strings[addr]) {
			return append(unit, addr);
		}
	}
	unit.strings.push_back(str);
	append(unit, addr);
}

void ncmacro::il::append(Unit & unit, string_t && str) {
	addr_t addr;
	for (addr = 0; addr < unit.strings.size(); ++addr) {
		if (str == unit.strings[addr]) {
			return append(unit, addr);
		}
	}
	unit.strings.push_back(std::move(str));
	append(unit, addr);
}

namespace ncmacro { /* g++5 complains without the namespaces */
namespace il {
template <>
void append<char const *>(Unit & unit, char const * const str) {
	append(unit, string_t{str});
}

template <typename T>
void append(Unit & unit, T const data) {
	for (size_t i = 0; i < sizeof(data); ++i) {
		unit.code.push_back(reinterpret_cast<char const *>(&data)[i]);
	}
}
} /* namespace ncmacro */
} /* namespace il */

void ncmacro::il::update(Unit & unit, addr_t const dst, addr_t const val) {
	for (size_t i = 0; i < sizeof(val); ++i) {
		unit.code[dst + i] = reinterpret_cast<char const *>(&val)[i];
	}
}

void ncmacro::il::ifelse(Unit & unit, std::function<void()> const doif) {
	append(unit, OpCode::NOT, OpCode::IF, OpCode::GOTO);
	addr_t const addr = unit.code.size();
	append(unit, addr_t{0});
	doif();
	update(unit, addr, static_cast<addr_t>(unit.code.size()));
}

void ncmacro::il::ifelse(Unit & unit, std::function<void()> const doif,
                         std::function<void()> const doelse) {
	append(unit, OpCode::IF, OpCode::GOTO);
	addr_t const gotoifaddr = unit.code.size();
	append(unit, addr_t{0});

	/* else block */
	doelse();
	append(unit, OpCode::GOTO);
	addr_t const gotoendaddr = unit.code.size();
	append(unit, addr_t{0});

	/* if block */
	update(unit, gotoifaddr, static_cast<addr_t>(unit.code.size()));
	doif();

	update(unit, gotoendaddr, static_cast<addr_t>(unit.code.size()));
}

std::ostream & ncmacro::il::operator <<(std::ostream & out, Unit const & unit) {
	::printcode(out, unit, std::begin(unit.code), std::end(unit.code));
	return out;
}

void ncmacro::il::run(Unit const & unit, Callback & callback) {
	Memory global;
	run(unit, callback, global);
}
