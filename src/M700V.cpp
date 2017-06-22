#include "M700V.hpp"

#include "IL.hpp"

#include <fstream>
#include <unordered_map>
#include <sstream>
#include <type_traits>
#include <memory>

#include <cassert>

using namespace ncmacro::m700v;

namespace {

using namespace std::string_literals;

using ncmacro::Block;

using ncmacro::string_t;
using ncmacro::addr_t;
using ncmacro::value_t;
using ncmacro::word_t;

using ncmacro::il::Unit;
using ncmacro::il::OpCode;

using ncmacro::il::append;
using ncmacro::il::update;
using ncmacro::il::ifelse;

struct PgmCall {
	addr_t nameid; /**< Name of the program to call. */
	addr_t addr;   /**< The address to point to the program. */
};

struct WhileAddr {
	addr_t id;
	addr_t loop;
	addr_t end;
};

struct Meta {
	std::unordered_map<string_t, addr_t> parsed;
	FileNames wanted;
	std::vector<PgmCall> calls;
	std::vector<WhileAddr> whiledo;
};

constexpr size_t const MAXLINE{65536};

template <typename T, typename ET = typename std::underlying_type<T>::type>
constexpr ET to_value(T const val) {
	return static_cast<ET>(val);
}

template <size_t Size>
constexpr bool is_in(char const needle, char const (&hay)[Size]) {
	for (auto const ch : hay) {
		if (needle == ch) { return ch > 0; }
	}
	return false;
}

constexpr bool is_num(char const ch) {
	return ch >= '0' && ch <= '9';
}

/**
 * Return number of matching characters if the complete reference string
 * matches the start of str.
 */
constexpr size_t match_start(char const * str, char const * const ref) {
	size_t i = 0;
	for (; str[i] && ref[i]; ++i) {
		if (str[i] != ref[i]) {
			return 0;
		}
	}
	if (ref[i] != 0) {
		return 0;
	}
	return i;
}

/**
 * Return number of matching characters one of the reference stings
 * matches the start of str.
 */
template <size_t ArrSize>
constexpr size_t match_start(char const * const str,
                             char const * const (&refs)[ArrSize]) {
	for (size_t i = 0; i < ArrSize && refs[i]; ++i) {
		auto cnt = match_start(str, refs[i]);
		if (cnt) {
			return cnt;
		}
	}
	return 0;
}

template <typename... StrTs>
constexpr size_t match_start(char const * const str, StrTs const... refs) {
	for (auto const ref : {refs...}) {
		auto cnt = match_start(str, ref);
		if (cnt) {
			return cnt;
		}
	}
	return 0;
}

addr_t extract_addr(char const * & it, size_t const size) {
	auto const end = it + size;
	addr_t result = 0;
	for (; it != end; ++it) {
		result = result * 10 + (*it - '0');
	}
	return result;
}

/**
 * Returns true if all parsers succeed.
 */
template <class...>
struct Seq {
	template <typename... ArgTs>
	constexpr Seq(ArgTs const &...) {}

	constexpr bool operator ()(char const *) const {
		return true;
	}
};

template <class Head, class... Tail>
struct Seq<Head, Tail...> {
	Head head;
	Seq<Tail...> tail;

	template <typename... ArgTs>
	constexpr Seq(ArgTs &... args) : head{args...}, tail{args...} {}

	bool operator ()(char const * & it) {
		return head(it) && tail(it);
	}
};

/**
 * Returns true on the first parser to succeed.
 */
template <class...>
struct Any {
	template <typename... ArgTs>
	constexpr Any(ArgTs const &...) {}

	constexpr bool operator ()(char const *) const {
		return false;
	}
};

template <class Head, class... Tail>
struct Any<Head, Tail...> {
	Head head;
	Any<Tail...> tail;

	template <typename... ArgTs>
	constexpr Any(ArgTs &... args) : head{args...}, tail{args...} {}

	bool operator ()(char const * & it) {
		return head(it) || tail(it);
	}
};

/**
 * Return true even when sequence fails.
 */
template <class... Ts>
struct Opt : Seq<Ts...> {
	using Seq<Ts...>::Seq;
	bool operator ()(char const * & it) {
		Seq<Ts...>::operator ()(it);
		return true;
	}
};

/**
 * Parse sequence until failure, always returns true.
 */
template <class... Ts>
struct All : Seq<Ts...> {
	using Seq<Ts...>::Seq;
	bool operator ()(char const * & it) {
		while (Seq<Ts...>::operator ()(it));
		return true;
	}
};

/**
 * Parse sequence until failure, returns true if at least one
 * sequence succeeded.
 */
template <class... Ts>
struct AllOnce : Seq<Ts...> {
	using Seq<Ts...>::Seq;
	bool operator ()(char const * & it) {
		if (!Seq<Ts...>::operator ()(it)) { return false; };
		while (Seq<Ts...>::operator ()(it));
		return true;
	}
};

struct Parse {
	Meta & meta;
	Unit & unit;
	string_t const & file;
	Parse(Meta & meta, Unit & unit, string_t const & file) :
	    meta{meta}, unit{unit}, file{file} {}
};

struct WhiteSpace : Parse {
	using Parse::Parse;
	bool operator ()(char const * & it) {
		for (; is_in(*it, " \t\r\n"); ++it);
		return true;
	}
};

struct Comment : WhiteSpace {
	using WhiteSpace::WhiteSpace;
	bool operator ()(char const * & it) {
		WhiteSpace::operator ()(it);
		switch (*it) {
		case '%':
			append(unit, OpCode::PRINT, string_t{it++, 1});
			return true;
		case '(':
			break;
		default:
			return false;
		}

		size_t count = 1;
		for (; it[count] && it[count] != ')'; ++count);
		if (it[count] == ')') {
			append(unit, OpCode::PRINT, string_t{it, count + 1});
			it += count + 1;
			return true;
		}
		return false;
	}
};

struct Label : Parse {
	All<Comment> comment;
	template <class... ArgTs> Label(ArgTs &... args) :
		Parse{args...}, comment{args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		if (*it != 'N') { return false; }

		++it;
		size_t count = 0;
		for (; is_num(it[count]); ++count);
		append(unit, OpCode::LBL, extract_addr(it, count));
		return true;
	}
};

struct Literal : Parse {
	All<Comment> comment;
	template <class... ArgTs> Literal(ArgTs &... args) :
		Parse{args...}, comment{args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		if (*it != '.' && (*it < '0' || *it > '9')) {
			return false;
		}
		value_t value{0};
		for (; *it >= '0' && *it <= '9'; ++it) {
			value = value * 10 + (*it - '0');
		}
		if (*it != '.') {
			append(unit, OpCode::LOAD, value);
			return true;
		}
		++it;
		value_t num{0};
		value_t denom{1};
		for (; *it >= '0' && *it <= '9'; ++it) {
			num = num * 10 + (*it - '0');
			denom = denom * 10;
		}
		value += num / denom;
		append(unit, OpCode::LOAD, value);
		return true;
	}
};

struct Var : Parse {
	All<Comment> comment;
	template <class... ArgTs> Var(ArgTs &... args) :
		Parse{args...}, comment{args...} {}

	bool operator ()(char const * & it,
	                 std::function<void(addr_t const)> const & local,
	                 std::function<void(addr_t const)> const & global) {
		comment(it);
		if (*it != '#') {
			return false;
		}
		auto ptr = it;
		comment(++ptr);
		size_t count = 0;
		for (; is_num(ptr[count]); ++count);
		if (!count) {
			return false;
		}
		auto const addr = extract_addr(ptr, count);
		it = ptr;
		if (addr < 34) {
			local(addr);
		} else {
			global(addr);
		}
		return true;
	}
};

struct LoadVar : Var {
	using Var::Var;
	std::function<void(addr_t const)> const local =
	    [&](addr_t const addr) { append(unit, OpCode::LLOAD, addr); };
	std::function<void(addr_t const)> const global =
	    [&](addr_t const addr) { append(unit, OpCode::GLOAD, addr); };
	bool operator ()(char const * & it) {
		return Var::operator ()(it, local, global);
	}
};

struct AssignVar : Var {
	using Var::Var;
	std::function<void(addr_t const)> const local =
	    [&](addr_t const addr) { append(unit, OpCode::LSTORE, addr); };
	std::function<void(addr_t const)> const global =
	    [&](addr_t const addr) { append(unit, OpCode::GSTORE, addr); };
	bool operator ()(char const * & it) {
		return Var::operator ()(it, local, global);
	}
};

std::unordered_map<string_t, OpCode> const opmap{
	{"*",     OpCode::MUL},
	{"/",     OpCode::DIV},
	{"MOD",   OpCode::MOD},
	{"AND",   OpCode::AND},
	{"+",     OpCode::ADD},
	{"-",     OpCode::SUB},
	{"OR",    OpCode::OR},
	{"XOR",   OpCode::XOR},
	{"EQ",    OpCode::EQ},
	{"NE",    OpCode::NE},
	{"GT",    OpCode::GT},
	{"LT",    OpCode::LT},
	{"GE",    OpCode::GE},
	{"LE",    OpCode::LE},
	{"SIN",   OpCode::SIN},
	{"COS",   OpCode::COS},
	{"TAN",   OpCode::TAN},
	{"ASIN",  OpCode::ASIN},
	{"ATAN",  OpCode::ATAN},
	{"ACOS",  OpCode::ACOS},
	{"SQRT",  OpCode::SQRT},
	{"SQR",   OpCode::SQRT},
	{"ABS",   OpCode::ABS},
	{"BIN",   OpCode::FROMBCD},
	{"BCD",   OpCode::TOBCD},
	{"ROUND", OpCode::ROUND},
	{"RND",   OpCode::ROUND},
	{"FIX",   OpCode::TRUNC},
	{"FUP",   OpCode::PAD},
	{"LN",    OpCode::LN},
	{"EXP",   OpCode::EXP},
};

struct Expr_base {
	virtual bool operator ()(char const * &) = 0;
};

struct Braced : Parse {
	All<Comment> comment;
	Expr_base & expr;

	template <typename... ArgTs>
	Braced(Expr_base & expr, ArgTs &... args) :
	    Parse{args...}, comment{args...}, expr{expr} {}

	bool operator ()(char const * & it) {
		comment(it);
		if (*it != '[') {
			return false;
		}
		auto ptr = it + 1;
		expr(ptr);
		comment(ptr);
		if (*ptr != ']') {
			return false;
		}
		it = ptr + 1;
		return true;
	}
};

struct Function : Parse {
	All<Comment> comment;
	Braced braced;

	template <typename... ArgTs>
	Function(Expr_base & expr, ArgTs &... args) :
	    Parse{args...}, comment{args...}, braced{expr, args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		size_t cnt = 0;
		for (; it[cnt] >= 'A' && it[cnt] <= 'Z'; ++cnt);

		string_t match{it, cnt};
		auto opcode = opmap.find(match);

		if (opcode == std::end(opmap)) {
			return false;
		}

		auto ptr = it + cnt;
		comment(ptr);
		if (!braced(ptr)) {
			return false;
		}
		it = ptr;

		switch (opcode->second) {
		case OpCode::SIN:
		case OpCode::COS:
		case OpCode::TAN:
			append(unit, OpCode::FROMDEG);
			break;
		case OpCode::SQRT:
		case OpCode::LN:
			append(unit, OpCode::CLONE, addr_t{1});
			append(unit, OpCode::LOAD0);
			append(unit, OpCode::LT);
			append(unit, OpCode::IF, OpCode::ERROR, "P282 Calculation impossible");
			break;
		default:
			break;
		}
		append(unit, opcode->second);
		switch (opcode->second) {
		case OpCode::ASIN:
		case OpCode::ATAN:
		case OpCode::ACOS:
			append(unit, OpCode::TODEG);
			break;
		default:
			break;
		}
		return true;
	}
};

struct Value {
	Any<Literal, LoadVar> value;
	Any<Function, Braced> subexpr;

	template <typename... ArgTs>
	Value(Expr_base & expr, ArgTs &... args) :
	    value{args...}, subexpr{expr, args...} {}

	bool operator ()(char const * & it) {
		return value(it) || subexpr(it);
	}
};

struct OpUnary : Parse {
	using Parse::Parse;

	bool operator ()(char const * & it) {
		switch (*it) {
		case '-':
			append(unit, OpCode::NEG);
		case '+':
			break;
		default:
			return false;
		}
		++it;
		return true;
	}
};

struct OpBinary : Parse {
	using Parse::Parse;

	bool operator ()(char const * & it) {
		size_t cnt = 0;
		if (is_in(*it, "*/+-")) {
			cnt = 1;
		} else {
			for (; it[cnt] >= 'A' && it[cnt] <= 'Z'; ++cnt);
		}

		string_t match{it, cnt};
		auto opcode = opmap.find(match);

		if (opcode == std::end(opmap)) {
			return false;
		}

		switch (opcode->second) {
		case OpCode::DIV:
			append(unit, OpCode::CLONE, addr_t{1});
			append(unit, OpCode::LOAD0);
			append(unit, OpCode::EQ, OpCode::IF);
			append(unit, OpCode::ERROR, "P283 Divided by zero");
			append(unit, OpCode::DIV);
			break;
		case OpCode::EQ:
			append(unit, OpCode::CLONE, addr_t{2});
			append(unit, OpCode::EQ);
			append(unit, OpCode::FLIP, addr_t{3});
			append(unit, OpCode::EQI, OpCode::AND);
			break;
		case OpCode::NE:
			append(unit, OpCode::CLONE, addr_t{2});
			append(unit, OpCode::NE);
			append(unit, OpCode::FLIP, addr_t{3});
			append(unit, OpCode::NEI, OpCode::OR);
			break;
		default:
			append(unit, opcode->second);
			break;
		}

		it += cnt;
		return true;
	}
};

enum class TokenT {
	NIL, OP_UNARY, OP_MUL, OP_ADD, OP_COMP, VALUE
};

constexpr char const * const operators[][6] {
	{},                                    /**< TokenType::NIL */
	{"+", "-"},                            /**< TokenType::OP_UNARY */
	{"*", "/", "MOD", "AND"},              /**< TokenType::OP_MUL */
	{"+", "-", "OR", "XOR"},               /**< TokenType::OP_ADD */
	{"EQ", "NE", "GT", "LT", "GE", "LE"},  /**< TokenType::OP_COMP */
};

struct Token {
	TokenT type;
	char const * begin;
	char const * end;

	std::unique_ptr<Token> lhs;
	std::unique_ptr<Token> rhs;

	operator bool() const {
		return this->type != TokenT::NIL;
	}
};

struct Tokenise : Comment {
	using Comment::Comment;

	Token unary(char const * & it) {
		Comment::operator ()(it);
		auto const start = it;
		auto cnt =
		    match_start(it, operators[to_value(TokenT::OP_UNARY)]);
		if (cnt) {
			it += cnt;
			return Token{TokenT::OP_UNARY, start, it};
		}
		return Token{TokenT::NIL, nullptr, nullptr};
	}

	Token binary(char const * & it) {
		Comment::operator ()(it);
		auto const start = it;
		for (auto const type : {TokenT::OP_MUL, TokenT::OP_ADD,
		                        TokenT::OP_COMP}) {
			auto cnt = match_start(it, operators[to_value(type)]);
			if (cnt) {
				it += cnt;
				return Token{type, start, it};
			}
		}
		return Token{TokenT::NIL, nullptr, nullptr};
	}

	Token value(char const * & it) {
		Comment::operator ()(it);
		auto const start = it;
		if (*it == '#') {
			auto ptr = it + 1;
			Comment::operator ()(ptr);
			if (is_num(*ptr)) {
				while (is_num(*++ptr));
				it = ptr;
				return Token{TokenT::VALUE, start, it};
			}
		}
		if (is_num(*it)) {
			while (is_num(*++it));
			if (*it != '.') {
				return Token{TokenT::VALUE, start, it};
			}
		}
		if (*it == '.') {
			while (is_num(*++it));
			return Token{TokenT::VALUE, start, it};
		}
		auto ptr = it;
		if (auto cnt = match_start(ptr, "SIN", "COS", "TAN", "ASIN",
		                           "ATAN", "ACOS", "SQRT", "SQR",
		                           "ABS", "BIN", "BCD", "ROUND",
		                           "RND", "FIX", "FUP", "LN", "EXP")) {
			ptr += cnt;
			Comment::operator ()(ptr);
		}
		if (*ptr == '[') {
			int cnt = 1;
			for (size_t i = 1; ptr[i]; ++i) {
				switch (ptr[i]) {
				case '[':
					++cnt;
					break;
				case ']':
					if (!--cnt) {
						it = ptr + i + 1;
						return {TokenT::VALUE,
						        start, it};
					}
					break;
				}
			}
		}
		return Token{TokenT::NIL, nullptr, nullptr};
	}

};

struct Expr : Parse, Expr_base {
	Value value;
	OpUnary unary;
	OpBinary binary;
	Tokenise tokenise;

	template <class... ArgTs> Expr(ArgTs &... args) :
	    Parse{args...}, value{*this, args...}, unary{args...},
	    binary{args...}, tokenise{args...} {}

	virtual bool operator ()(char const * & it) override {
		auto ptr = it;
		std::vector<Token> tokens;

		/* tokenise the expression */
		Token token = tokenise.unary(ptr);
		if (token) {
			tokens.push_back(std::move(token));
		}

		while (true) {
			if ((token = tokenise.value(ptr))) {
				tokens.push_back(std::move(token));
			} else {
				return false;
			}

			if ((token = tokenise.binary(ptr))) {
				tokens.push_back(std::move(token));
			} else {
				break;
			}

			if (tokens.back().type == TokenT::OP_COMP &&
			    (token = tokenise.unary(ptr))) {
				tokens.push_back(std::move(token));
			}
		}
		it = ptr;

		/* build a tree */
		for (auto type : {TokenT::OP_MUL, TokenT::OP_ADD,
		                  TokenT::OP_COMP}) {
			/* bind unary operators */
			for (size_t i = 0;
			     type == TokenT::OP_ADD && i < tokens.size();
			     ++i) {
				if (tokens[i].type != TokenT::OP_UNARY) {
					continue;
				}
				tokens[i].rhs =
				    std::make_unique<Token>(std::move(tokens[i + 1]));
				tokens.erase(std::begin(tokens) + i + 1);
			}

			/* bind arguments to binary operators */
			for (size_t i = 1; i < tokens.size() - 1;) {
				if (tokens[i].type == type) {
					std::swap(tokens[i - 1], tokens[i]);
					auto & op = tokens[i - 1];
					op.lhs =
					    std::make_unique<Token>(
					        std::move(tokens[i]));
					op.rhs =
					    std::make_unique<Token>(
					        std::move(tokens[i + 1]));
					tokens.erase(std::begin(tokens) + i,
					             std::begin(tokens) + i + 2);
				} else {
					++i;
				}
			}
		}

		assert(tokens.size() == 1 && "must be a bug!");
		/* emit the code */
		auto const emit = [&](auto & self, Token & token) -> void {
			if (token.lhs) { self(self, *token.lhs); }
			if (token.rhs) { self(self, *token.rhs); }
			switch (token.type) {
			case TokenT::VALUE:
				value(token.begin);
				break;
			case TokenT::OP_UNARY:
				unary(token.begin);
				break;
			default:
				binary(token.begin);
				break;
			}
		};
		emit(emit, tokens[0]);

		return true;
	}
};

struct Assign : Parse {
	All<Comment> comment;
	Var var;
	AssignVar assign;
	Expr expr;

	template <class... ArgTs> Assign(ArgTs &... args) :
	    Parse{args...}, comment{args...}, var{args...}, assign{args...},
	    expr{args...} {}

	bool operator ()(char const * & it) {
		auto ptr = it;
		if (!var(ptr, [](addr_t const) {}, [](addr_t const) {})) {
			return false;
		}
		comment(ptr);
		if (*ptr != '=') {
			return false;
		}
		++ptr;
		if (!expr(ptr)) {
			return false;
		}
		assign(it);
		it = ptr;
		return true;
	}
};

struct Goto : Parse {
	All<Comment> comment;
	Expr expr;

	template <class... ArgTs> Goto(ArgTs &... args) :
	    Parse{args...}, comment{args...}, expr{args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		auto cnt = match_start(it, "GOTO");
		if (!cnt) { return false; }
		auto ptr = it + cnt;
		if (!expr(ptr)) { return false; }
		it = ptr;
		append(unit, OpCode::LOADSTR, string_t{"P231 No squence No."});
		append(unit, OpCode::FLIP, addr_t{2});
		append(unit, OpCode::GOTOLBL);
		return true;
	}
};

struct If : Parse {
	All<Comment> comment;
	Expr expr;
	Braced braced;
	Goto go;

	template <class... ArgTs> If(ArgTs &... args) :
	    Parse{args...}, comment{args...}, expr{args...},
	    braced{expr, args...}, go{args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		auto cnt = match_start(it, "IF");
		if (!cnt) { return false; }
		auto ptr = it + cnt;
		if (!braced(ptr)) { return false; }
		it = ptr;
		ifelse(unit, [&]() {
			if (!go(it)) {
				append(unit, OpCode::WARN, "failed to parse GOTO behind IF");
			}
		});
		return true;
	}
};

struct While : Parse {
	All<Comment> comment;
	Expr expr;
	Braced braced;

	template <class... ArgTs> While(ArgTs &... args) :
	    Parse{args...}, comment{args...}, expr{args...},
	    braced{expr, args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		auto cnt = match_start(it, "WHILE");
		if (!cnt) { return false; }
		auto ptr = it + cnt;
		addr_t const begin = unit.code.size();
		if (!braced(ptr)) { return false; }
		comment(ptr);
		cnt = match_start(ptr, "DO");
		if (!cnt) { goto fail; }
		ptr += cnt;
		comment(ptr);
		cnt = 0;
		for (; is_num(ptr[cnt]); ++cnt);
		if (!cnt) { goto fail; }
		it = ptr + cnt;
		append(unit, OpCode::NOT, OpCode::IF, OpCode::GOTO);
		meta.whiledo.push_back({extract_addr(ptr, cnt), begin,
		                        static_cast<addr_t>(unit.code.size())});
		append(unit, addr_t{0});
		return true;
	fail:
		unit.code.erase(std::begin(unit.code) + begin,
		                std::end(unit.code));
		return false;
	}
};

struct End : Parse {
	All<Comment> comment;

	template <class... ArgTs> End(ArgTs &... args) :
	    Parse{args...}, comment{args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		auto cnt = match_start(it, "END");
		if (!cnt) { return false; }
		auto ptr = it + cnt;
		comment(ptr);
		cnt = 0;
		for (; is_num(ptr[cnt]); ++cnt);
		if (!cnt) { return false; }
		auto const id = extract_addr(ptr, cnt);
		it = ptr;
		append(unit, OpCode::GOTO, meta.whiledo.back().loop);
		update(unit, meta.whiledo.back().end,
		       static_cast<addr_t>(unit.code.size()));
		if (meta.whiledo.back().id != id) {
			append(unit, OpCode::ERROR, "DO/END mismatch");
		}
		meta.whiledo.pop_back();
		return true;
	}
};

struct Word : Parse {
	All<Comment> comment;
	Expr expr;

	template <class... ArgTs> Word(ArgTs &... args) :
	    Parse{args...}, comment{args...}, expr{args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		word_t name {""};
		if (it[0] == ',' && it[1] >= 'A' && it[1] <= 'Z') {
			name.str[0] = it[0];
			name.str[1] = it[1];
			it += 2;
		} else if (it[0] >= 'A' && it[0] <= 'Z') {
			name.str[0] = it[0];
			++it;
		} else {
			return false;
		}
		if (!expr(it)) {
			append(unit, OpCode::LOAD0);
		}
		append(unit, OpCode::BAPPEND, name);
		return true;
	}
};

struct String : Parse {
	All<Comment> comment;

	template <class... ArgTs> String(ArgTs &... args) :
	    Parse{args...}, comment{args...} {}

	bool operator ()(char const * & it) {
		comment(it);
		if (*it != '<') { return false; }
		size_t cnt = 1;
		for (; it[cnt] && it[cnt] != '>'; ++cnt);
		if (!it[cnt]) { return false; }
		string_t const str{it + 1, cnt - 1};
		it += cnt + 1;
		append(unit, OpCode::LOADSTR, str);
		append(unit, OpCode::BAPPEND, word_t{"STR"});
		meta.wanted.push_back(str);
		return true;
	}
};

struct Commit : Parse {
	using Parse::Parse;
	bool operator ()(char const * & it) {
		append(unit, OpCode::LCALL);
		meta.calls.push_back({unit.strmap["/checks/bcommit/"],
		                      static_cast<addr_t>(unit.code.size())});
		append(unit, addr_t{0});
		return true;
	}
};

void parse(Meta & meta, Unit & unit, string_t const & file) {
	/* Has the file already been parsed? */
	if (meta.parsed.count(file)) {
		return;
	}

	/* Register program. */
	meta.parsed[file] = unit.code.size();

	/* Binary mode prevents windows from messing with us. */
	std::ifstream in{file, std::ifstream::binary};
	if (!in) {
		addr_t const begin = unit.code.size();
		append(unit, OpCode::ERROR,
		       file + ": cannot be accessed");
		append(unit, OpCode::WRAPLBL, begin);
		return;
	}

	using Macro = Any<Assign, Goto, If, End, While>;
	using GCode = Seq<AllOnce<Any<Word, String>>, Commit>;
	Seq<Opt<Label>, Any<Macro, GCode>, All<Comment>>
	    lineparser{meta, unit, file};

	char buf[MAXLINE]{};
	for (size_t lineno = 1; in.getline(buf, sizeof(buf)); ++lineno) {
		char const * it = buf;
		
		lineparser(it);
		
		if (*it) {
			std::ostringstream stream{};
			stream << file << ':' << lineno << ':'
			       << (it - buf + 1)
			       << ": could not parse: " << it;
			append(unit, OpCode::ERROR, stream.str());
		}
	}

	append(unit, OpCode::WRAPLBL, meta.parsed[file]);
	while (meta.wanted.size()) {
		auto want = meta.wanted.back();
		meta.wanted.pop_back();
		parse(meta, unit, want);
	}
}

} /* namespace */

void ncmacro::m700v::parse(Unit & unit, FileNames const & files) {
	Meta meta;
	unit.strmap["/checks/bcommit/"] = unit.strings.size();
	unit.strings.push_back("/checks/bcommit/");

	for (auto const & file : files) {
		parse(meta, unit, file);
	}

	/* gcode commit code */

	meta.parsed["/checks/bcommit/"] = unit.code.size();

	/*
	 * Not passed on to the callback interface.
	 */

	/* M98 */
	append(unit, OpCode::BMATCH, word_t{"M"}, value_t{98});
	ifelse(unit, [&]() {
		auto const end = std::end(meta.parsed);
		for (auto it = std::begin(meta.parsed); it != end; ++it) {
			append(unit, OpCode::LOADSTR, it->first);
			append(unit, OpCode::BHAS, word_t{"P"}, OpCode::IF);
			append(unit, OpCode::ERROR,
			       string_t{"M98P is not supported, use M98<filename> instead."});
			append(unit, OpCode::BLOAD, word_t{"STR"}, addr_t{0});
			append(unit, OpCode::EQ);
			ifelse(unit, [&]() {
				/* put the number of repetitions on the stack */
				append(unit, OpCode::BHAS, word_t{"L"});
				ifelse(unit, [&]() {
					append(unit, OpCode::BLOAD,
					       word_t{"L"}, addr_t{0});
				}, [&]() {
					append(unit, OpCode::LOAD1);
				});
				/* are we jumping to a label ? */
				append(unit, OpCode::BHAS, word_t{"H"});
				ifelse(unit, [&]() {
					append(unit, OpCode::LOADSTR,
					       string_t{"P231 No squence No."});
					append(unit, OpCode::BLOAD, word_t{"H"},
					       addr_t{0}); 
					append(unit, OpCode::BFLUSH);
					/* put the repetitions on top */
					append(unit, OpCode::FLIP, addr_t{3});
					repeat(unit, [&]() {
						append(unit, OpCode::FLIP,
						       addr_t{3});
						append(unit, OpCode::CLONE,
						       addr_t{2});
						append(unit, OpCode::CALLLBL,
						       it->second);
						append(unit, OpCode::FLIP,
						       addr_t{3});
					});
					append(unit, OpCode::POP, addr_t{2});
				}, [&]() {
					append(unit, OpCode::BFLUSH);
					repeat(unit, [&]() {
						append(unit, OpCode::CALL, it->second);
					});
				});
				append(unit, OpCode::LRET);
			});
		}
		append(unit, OpCode::ERROR, "file called through M98 is not present");
	});
	/* M99 */
	append(unit, OpCode::BMATCH, word_t{"M"}, value_t{99});
	ifelse(unit, [&]() {
		/* pop return address */
		append(unit, OpCode::POP, addr_t{1});
		/* return as if from caller */
		append(unit, OpCode::RET);
	});

	/* Pass to callback interface. */
	append(unit, OpCode::BCOMMIT);

	/* M02/M30 */
	append(unit, OpCode::BMATCH, word_t{"M"}, value_t{30});
	append(unit, OpCode::BMATCH, word_t{"M"}, value_t{2});
	append(unit, OpCode::OR);
	ifelse(unit, [&]() {
		/* Pop return address from stack and exit. */
		append(unit, OpCode::POP, addr_t{1}, OpCode::EXIT);
	});

	/* Return to caller. */
	append(unit, OpCode::BFLUSH, OpCode::LRET);

	/* Update unresolved calls. */
	for (auto const & call : meta.calls) {
		auto & str = unit.strings[call.nameid];
		auto const it = meta.parsed.find(str);
		assert(it != std::end(meta.parsed) &&
		       "every file should exist, even missing files");
		update(unit, call.addr, it->second);
	}
}
