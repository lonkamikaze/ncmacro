#include "M700V.hpp"

#include <cassert>
#include <functional>
#include <istream>
#include <fstream>
#include <iomanip>
#include <cstdint>
#include <memory>
#include <unordered_map>
#include <vector>
#include <stack>
#include <regex>
#include <sstream>

#include "IL.hpp"

using namespace ncmacro::m700v;

namespace {

using ncmacro::addr_t;
using ncmacro::word_t;
using ncmacro::value_t;
using ncmacro::string_t;

using ncmacro::il::OpCode;
using ncmacro::il::Unit;

constexpr size_t const MAXLINE{65536};

class Guard {
	private:
	std::istream & in;
	std::streampos pos;
	std::ios_base::iostate state;

	public:
	Guard(std::istream & in) :
	    in{in}, pos{in.tellg()}, state{in.rdstate()} {}

	~Guard() {
		this->in.clear();
		this->in.setstate(this->state);
		this->in.seekg(this->pos);
	}

	void commit() {
		this->state = this->in.rdstate();
		this->pos = this->in.tellg();
	}

	std::streampos getPos() const {
		return this->pos;
	}
};

inline std::regex operator ""_r(char const * const literal, size_t const) {
	return std::regex{literal};
}

std::string fetch(std::istream & in, std::regex const & expr) {
	char buf[MAXLINE]{0};
	{
		Guard guard{in};
		in.get(buf, sizeof(buf), '\n');
	}
	std::cmatch m;
	if (!std::regex_search(buf, m, expr)) {
		return "";
	}
	in.seekg(m[0].length(), std::ios_base::cur);
	return m[0].str();
}

enum class TType {
	NIL, ENDFILE, ENDLINE, COMMENT, KEYWORD, BRACKET_SQU_OPEN,
	BRACKET_SQU_CLOSE, OP_MUL, OP_ADD, OP_UNARY, OP_COMP, OP_ASSIGN,
	UINT, VAL, STRING, C_VAR, C_ASSIGN, C_LABEL, WORD
};

struct Token {
	TType type{TType::NIL};
	std::streampos start;
	std::streampos end;
	std::string str;

	operator bool() const {
		return this->type != TType::NIL;
	}
};

/**
 * Takes an istream and turns it into tokens.
 */
struct Lexer {
	std::istream & in;

	/**
	 * Skips white space and comments.
	 */
	void skipws() {
		while (fetch(this->in, "^[\t\r ]*"_r) != "" ||
		       fetch(this->in, "^\\([^)]*\\)"_r) != "");
	}

	/**
	 * Fetch token by regex.
	 */
	template <TType Type>
	Token get(std::regex const & expr) {
		if (this->in.eof()) {
			return {};
		}
		Guard guard{this->in};
		skipws();
		auto start = this->in.tellg();
		auto str = fetch(this->in, expr);
		if (str == "") {
			return {};
		}
		guard.commit();
		auto end = guard.getPos();
		return {Type, start, end, str};
	}

	Lexer(std::istream & in) : in{in} {}

	template <TType Head, TType... Tail>
	Token get() {
		Token token = get<Head>();
		if (token.type != TType::NIL) {
			return token;
		}
		return get<Tail...>();
	}
};

template <> Token Lexer::get<TType::ENDFILE>() {
	Guard guard{this->in};
	skipws();
	this->in.get();
	if (!this->in.eof()) {
		return {};
	}
	guard.commit();
	auto pos = guard.getPos();
	return {TType::ENDFILE, pos, pos, ""};
}

template <> Token Lexer::get<TType::ENDLINE>() {
	if (this->in.eof()) {
		return {};
	}
	Guard guard{this->in};
	skipws();
	auto start = this->in.tellg();
	while (this->in.get() != '\n') {
		return {};
	}
	guard.commit();
	auto end = guard.getPos();
	return {TType::ENDLINE, start, end, "\n"};
}

template <> Token Lexer::get<TType::COMMENT>() {
	if (this->in.eof()) {
		return {};
	}
	Guard guard{this->in};
	fetch(this->in, "^[\t\r ]*"_r);
	auto start = this->in.tellg();
	auto str = fetch(this->in, "^\\([^)]*\\)"_r);
	if (str == "") {
		return {};
	}
	guard.commit();
	auto end = guard.getPos();
	return {TType::COMMENT, start, end, str};
}

template <> Token Lexer::get<TType::KEYWORD>() {
	return get<TType::KEYWORD>("^[A-Z]+"_r);
}

template <> Token Lexer::get<TType::BRACKET_SQU_OPEN>() {
	return get<TType::BRACKET_SQU_OPEN>("^\\["_r);
}

template <> Token Lexer::get<TType::BRACKET_SQU_CLOSE>() {
	return get<TType::BRACKET_SQU_CLOSE>("^\\]"_r);
}

template <> Token Lexer::get<TType::OP_MUL>() {
	return get<TType::OP_MUL>("^([*/]|MOD|AND)"_r);
}

template <> Token Lexer::get<TType::OP_ADD>() {
	return get<TType::OP_ADD>("^([-+]|OR|XOR)"_r);
}

template <> Token Lexer::get<TType::OP_COMP>() {
	return get<TType::OP_COMP>("^(EQ|NE|GT|LT|GE|LE)"_r);
}

template <> Token Lexer::get<TType::OP_ASSIGN>() {
	return get<TType::OP_ASSIGN>("^="_r);
}

template <> Token Lexer::get<TType::OP_UNARY>() {
	return get<TType::OP_UNARY>("^[-+]"_r);
}

template <> Token Lexer::get<TType::UINT>() {
	return get<TType::UINT>("^[0-9]+"_r);
}

template <> Token Lexer::get<TType::VAL>() {
	return get<TType::VAL>("^([0-9]+\\.?[0-9]*|\\.[0-9]+)"_r);
}

template <> Token Lexer::get<TType::STRING>() {
	return get<TType::STRING>("^<[^>]*>"_r);
}

template <> Token Lexer::get<TType::C_VAR>() {
	return get<TType::C_VAR>("^#"_r);
}

template <> Token Lexer::get<TType::C_ASSIGN>() {
	return get<TType::C_ASSIGN>("^="_r);
}

template <> Token Lexer::get<TType::C_LABEL>() {
	return get<TType::C_LABEL>("^N"_r);
}

template <> Token Lexer::get<TType::WORD>() {
	return get<TType::WORD>("^,?[A-Z]"_r);
}

struct Pgm {
	string_t name;
	addr_t addr;
};

using Pgms = std::vector<Pgm>;

/**
 * Compile time meta.
 */
struct AddrMeta {
	Pgms pgms;  /**< Contains program names and addresses. */
	Pgms calls; /**< Programm names and addresses of calls. */
};

struct {
	char const * const c_str;
	OpCode op;
} const opmap[]{
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

struct Node {
	Lexer & lexer;
	Guard guard{lexer.in};

	bool constructed{false};

	Node(Lexer & lexer) : lexer{lexer} {}

	void construct(bool success) {
		this->constructed = success;
		if (success) {
			this->guard.commit();
		}
	}

	virtual TType type() const { return TType::NIL; }

	/**
	 * Traverse syntax tree and emit code.
	 */
	virtual void emit(AddrMeta &, Unit &) const = 0;
};

using Node_ptr = std::unique_ptr<Node>;

template <class NodeT, typename... ArgTs>
inline bool make(Node_ptr & dst, ArgTs &... args) {
	NodeT node{args...};
	if (!node.constructed) {
		return false;
	}
	dst = Node_ptr{new NodeT{std::move(node)}};
	return true;
}

template <class...>
struct any {
	template <typename... ArgTs>
	any(Node_ptr &, ArgTs &...) {}

	operator bool() const {
		return false;
	}
};

template <class NodeT, class... Tail>
struct any<NodeT, Tail...> {
	Node_ptr & dst;

	template <typename... ArgTs>
	any(Node_ptr & dst, ArgTs &... args) : dst{dst} {
		if (!make<NodeT>(dst, args...)) {
			any<Tail...>(dst, args...);
		}
	}

	operator bool() const {
		return this->dst != nullptr;
	}
};

/*
 * macro syntax tree nodes
 */

struct m_literal : public Node {
	Token token;
	value_t value;

	m_literal(Lexer & lexer) : Node{lexer},
	    token{lexer.get<TType::VAL>()} {
		if (this->token) {
			std::istringstream{this->token.str} >> this->value;
		}
		construct(this->token);
	}

	virtual void emit(AddrMeta &, Unit & unit) const override {
		append(unit, OpCode::LOAD, this->value);
	}
};

struct m_var : public Node {
	Token c_var, uint;
	addr_t ref;

	m_var(Lexer & lexer) : Node{lexer},
	    c_var{lexer.get<TType::C_VAR>()} {
		if (!this->c_var) {
			return;
		}
		this->uint = lexer.get<TType::UINT>();
		if (this->uint) {
			std::istringstream{this->uint.str} >> this->ref;
			construct(true);
		}
	}

	bool isLocal() const {
		return this->ref < 34;
	}

	virtual void emit(AddrMeta &, Unit & unit) const override {
		if (this->ref < 34) {
			append(unit, OpCode::LLOAD, this->ref);
		} else /*if (this->ref < 200 ||
		           (this->ref >= 500 && this->ref < 1000))*/ {
			append(unit, OpCode::GLOAD, this->ref);
		}
	}
};

struct m_expr;
struct m_function;
struct m_brackets;
struct m_op_unary;
struct m_op_binary;

struct m_value : public Node {
	Node_ptr child;

	m_value(Lexer & lexer) : Node{lexer} {
		construct(any<m_var, m_literal, m_brackets, m_function>
		              (this->child, lexer));
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->child->emit(meta, unit);
	}
};

struct m_op_binary : public Node {
	Node_ptr lhs, rhs;
	Token token;
	OpCode opcode;
	m_op_binary(Lexer & lexer) : Node{lexer} {
		this->token = lexer.get<TType::OP_MUL, TType::OP_ADD, TType::OP_COMP>();
		if (!this->token) {
			return;
		}

		for (auto & oppair : opmap) {
			if (this->token.str == oppair.c_str) {
				this->opcode = oppair.op;
				construct(true);
				break;
			}
		}
	}

	virtual TType type() const override { return this->token.type; }

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->lhs->emit(meta, unit);
		this->rhs->emit(meta, unit);
		switch (this->opcode) {
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
			append(unit, this->opcode);
			break;
		}
	}
};

struct m_op_unary : public Node {
	Node_ptr rhs;
	Token token;
	OpCode opcode;
	m_op_unary(Lexer & lexer) : Node{lexer},
	    token{lexer.get<TType::OP_UNARY>()} {
		if        (this->token.str == "+") {
			this->opcode = OpCode::NOP;
		} else if (this->token.str == "-") {
			this->opcode = OpCode::NEG;
		} else {
			return;
		}
		construct(true);
	}

	virtual TType type() const override { return this->token.type; }

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->rhs->emit(meta, unit);
		if (this->opcode != OpCode::NOP) {
			append(unit, this->opcode);
		}
	}
};

struct m_brackets : public Node {
	Token open;
	Node_ptr expr;
	Token close;

	m_brackets(Lexer & lexer) : Node{lexer},
	    open{lexer.get<TType::BRACKET_SQU_OPEN>()} {
	    	if (!this->open) {
	    		return;
	    	}
	    	if (!make<m_expr>(this->expr, lexer)) {
	    		return;
	    	}
		this->close = lexer.get<TType::BRACKET_SQU_CLOSE>();
		construct(this->close);
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->expr->emit(meta, unit);
	}
};

struct m_function : public Node {
	Token name;
	Node_ptr arg;
	OpCode opcode;
	m_function(Lexer & lexer) : Node{lexer},
	    name{lexer.get<TType::KEYWORD>()} {
		if (!this->name) { return; }

		for (auto & oppair : opmap) {
			if (this->name.str == oppair.c_str) {
				this->opcode = oppair.op;
				construct(make<m_brackets>(this->arg, lexer));
				break;
			}
		}
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->arg->emit(meta, unit);
		switch (this->opcode) {
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
		append(unit, this->opcode);
		switch (this->opcode) {
		case OpCode::ASIN:
		case OpCode::ATAN:
		case OpCode::ACOS:
			append(unit, OpCode::TODEG);
			break;
		default:
			break;
		}
	}
};

struct m_expr : public Node {
	Node_ptr node;

	m_expr(Lexer & lexer) : Node{lexer} {
		/* create an alternating list of operators and values */
		std::vector<Node_ptr> nodes;
		Node_ptr node;
		if (make<m_op_unary>(node, lexer)) {
			nodes.push_back(std::move(node));
		}

		while (true) {
			if (make<m_value>(node, lexer)) {
				nodes.push_back(std::move(node));
			} else {
				return;
			}
			if (make<m_op_binary>(node, lexer)) {
				nodes.push_back(std::move(node));
			} else {
				break;
			}
			/* unary operators may appear behind comparison
			 * operators */
			if (nodes.back()->type() == TType::OP_COMP &&
			    make<m_op_unary>(node, lexer)) {
				nodes.push_back(std::move(node));
			}
		}

		/* bind operators in order of preference */
		for (auto type : {TType::OP_MUL, TType::OP_ADD, TType::OP_COMP}) {
			/* bind rhs to unary operator */
			for (size_t i = 0;
			     type == TType::OP_ADD && i < nodes.size() - 1;
			     ++i) {
				if (nodes[i]->type() == TType::OP_UNARY) {
					auto op = static_cast<m_op_unary *>(nodes[i].get());
					op->rhs = std::move(nodes[i + 1]);
					nodes.erase(nodes.begin() + i + 1);
				}
			}

			/* bind lhs and rhs to operator */
			for (size_t i = 1; i < nodes.size() - 1;) {
				if (nodes[i]->type() == type) {
					std::swap(nodes[i - 1], nodes[i]);
					auto op = static_cast<m_op_binary *>(nodes[i - 1].get());
					op->lhs = std::move(nodes[i]);
					op->rhs = std::move(nodes[i + 1]);
					nodes.erase(nodes.begin() + i,
					            nodes.begin() + i + 2);
				} else {
					++i;
				}
			}
		}

		/* finalise node */
		if (nodes.size() == 1) {
			this->node = std::move(nodes[0]);
			construct(true);
		}
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->node->emit(meta, unit);
	}
};

struct m_assign : public Node {
	m_var lhs;
	Token token;
	OpCode opcode;
	m_expr rhs;

	m_assign(Lexer & lexer) : Node{lexer},
	    lhs{lexer}, token{lexer.get<TType::C_ASSIGN>()}, rhs{lexer} {
		construct(this->lhs.constructed &&
		          this->token &&
		          this->rhs.constructed);
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->rhs.emit(meta, unit);
		if (this->lhs.isLocal()) {
			append(unit, OpCode::LSTORE);
		} else {
			append(unit, OpCode::GSTORE);
		}
		append(unit, addr_t{this->lhs.ref});
	}
};

struct m_comment : public Node {
	Token token;
	m_comment(Lexer & lexer) : Node{lexer},
	    token{lexer.get<TType::COMMENT>()} {
		construct(this->token);
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		append(unit, OpCode::PRINT, this->token.str);
	}
};

struct block;

struct m_while : public Node {
	Token cmd;
	Node_ptr expr;
	Token branch, branchval, terminator;
	Node_ptr child;
	Token end, endval;

	m_while(Lexer & lexer) : Node{lexer},
	    cmd{lexer.get<TType::KEYWORD>()} {
		if (!(this->cmd &&
		      this->cmd.str == "WHILE" &&
		      make<m_brackets>(this->expr, lexer) &&
		      (this->branch = lexer.get<TType::KEYWORD>()) &&
		      this->branch.str == "DO" &&
		      (this->branchval = lexer.get<TType::UINT>()) &&
		      (this->terminator = lexer.get<TType::ENDLINE>()))) {
		      	return;
		}
		make<block>(this->child, lexer);
		if ((this->end = lexer.get<TType::KEYWORD>()) &&
		    this->end.str == "END" &&
		    (this->endval = lexer.get<TType::UINT>()) &&
		    this->endval.str == this->branchval.str) {
			construct(true);
		}
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		addr_t const begin = unit.code.size();
		this->expr->emit(meta, unit);
		append(unit, OpCode::NOT, OpCode::IF, OpCode::GOTO);
		addr_t const gotoend = unit.code.size();
		append(unit, addr_t{0});
		if (this->child != nullptr) {
			this->child->emit(meta, unit);
		}
		append(unit, OpCode::GOTO, begin);
		addr_t const end = unit.code.size();
		update(unit, gotoend, end);
	}
};

struct m_label : public Node {
	Token label, lval;
	addr_t value;
	m_label(Lexer & lexer) : Node{lexer},
	    label{lexer.get<TType::C_LABEL>()},
	    lval{lexer.get<TType::UINT>()} {
		if (this->label && this->lval) {
			std::istringstream{lval.str} >> this->value;
			construct(true);
		}
	}

	virtual void emit(AddrMeta &, Unit & unit) const override {
		append(unit, OpCode::LBL, this->value);
	}
};

struct m_goto : public Node {
	Token token;
	Node_ptr value;
	m_goto(Lexer & lexer) : Node{lexer},
	    token{lexer.get<TType::KEYWORD>()} {
		if (!this->token || this->token.str != "GOTO") {
			return;
		}
		construct(make<m_value>(this->value, lexer));
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->value->emit(meta, unit);
		append(unit, OpCode::GOTOLBL);
	}
};

struct m_if : public Node {
	Token token;
	Node_ptr expr;
	Node_ptr go;
	m_if(Lexer &  lexer) : Node{lexer},
	    token{lexer.get<TType::KEYWORD>()} {
		if (!this->token || this->token.str != "IF") { return; }
		construct(make<m_brackets>(this->expr, lexer) &&
		          make<m_goto>(this->go, lexer));
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		this->expr->emit(meta, unit);
		ifelse(unit, [&]() { this->go->emit(meta, unit); });
	}
};

/*
 * ISO style blocks.
 */

/**
 * Keywords that must not be matched as words.
 */
struct notwords : public Node {
	Token token;
	
	notwords(Lexer & lexer) : Node{lexer},
	    token{lexer.get<TType::KEYWORD>()} {
		construct(this->token && this->token.str == "END");
	}

	virtual void emit(AddrMeta &, Unit &) const override {}
};

struct endwords : public Node {
	Token token;

	endwords(Lexer & lexer) : Node{lexer},
	    token{lexer.get<TType::ENDLINE>()} {
		construct(this->token);
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		append(unit, OpCode::LCALL);
		meta.calls.push_back({"/CHECKS/BCOMMIT/",
		                      static_cast<addr_t>(unit.code.size())});
		append(unit, addr_t{0});
	}
};

struct words : public Node {
	Token token;
	Node_ptr expr;
	Node_ptr next;

	words(Lexer & lexer) : Node{lexer},
	    token{lexer.get<TType::WORD, TType::STRING>()} {
		if (!this->token) { return; }
		switch (this->token.type) {
		case TType::WORD:
			make<m_expr>(this->expr, lexer);
			break;
		default:
			break;
		}
		construct(any<words, endwords>(this->next, lexer));
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		switch (this->token.type) {
		case TType::WORD:
			if (this->expr) {
				this->expr->emit(meta, unit);
			} else {
				append(unit, OpCode::LOAD0);
			}
			{
				word_t name{0};
				for (size_t i = 0; i < this->token.str.size(); ++i) {
					name.str[i] = this->token.str[i];
				}
				append(unit, OpCode::BAPPEND, name);
			}
			break;
		case TType::STRING:
			append(unit, OpCode::LOADSTR,
			       string_t{std::begin(this->token.str) + 1,
			                std::end(this->token.str) - 1});
			append(unit, OpCode::BAPPEND, word_t{"STR"});
			break;
		default:
			break;
		}
		if (this->next) {
			this->next->emit(meta, unit);
		}
	}
};

/*
 * block node
 */

struct block : public Node {
	Node_ptr node;
	Token terminator;
	Node_ptr next;

	block(Lexer & lexer) : Node{lexer} {
		/* termination optional */
		if (any<m_label, m_comment>(this->node, lexer)) {
			this->terminator = lexer.get<TType::ENDLINE>();
			make<block>(this->next, lexer);
			construct(true);
			return;
		}
		/* empty line */
		if ((this->terminator = lexer.get<TType::ENDLINE>())) {
			make<block>(this->next, lexer);
			construct(true);
			return;
		}
		/* termination obligatory */
		if (any<m_assign, m_while, m_goto, m_if>(this->node, lexer) &&
		    (this->terminator = lexer.get<TType::ENDLINE>())) {
			make<block>(this->next, lexer);
			construct(true);
			return;
		}
		/* bail on certtain keywords */
		if (make<notwords>(this->node, lexer)) { return; }
		/* match gcode blocks */
		if (make<words>(this->node, lexer)) {
			make<block>(this->next, lexer);
			construct(true);
			return;
		}
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		if (this->constructed) {
			if (this->node) {
				this->node->emit(meta, unit);
			}
			if (this->next) {
				this->next->emit(meta, unit);
			}
		}
	}
};

struct pgm : Node {
	Node_ptr child;
	Token eof;

	pgm(Lexer & lexer) : Node{lexer} {
		construct(make<block>(this->child, lexer));
		if (this->child) {
			this->eof = lexer.get<TType::ENDFILE>();
		}
	}

	virtual void emit(AddrMeta & meta, Unit & unit) const override {
		if (this->constructed) {
			this->child->emit(meta, unit);
			if (!this->eof) {
				append(unit, OpCode::WARN,
				       meta.pgms.back().name +
				       ": could not be parsed to the end");
			}
			append(unit, OpCode::WRAPLBL, meta.pgms.back().addr);
		}
	}
};

void finalise(AddrMeta & meta, Unit & unit) {
	meta.pgms.push_back({"/CHECKS/BCOMMIT/",
	                     static_cast<addr_t>(unit.code.size())});

	/*
	 * Not passed on to the callback interface.
	 */
	/* M98 */
	append(unit, OpCode::BMATCH, word_t{"M"}, value_t{98});
	ifelse(unit, [&]() {
		for (auto const & pgm : meta.pgms) {
			append(unit, OpCode::LOADSTR, pgm.name);
			append(unit, OpCode::BLOAD, word_t{"STR"}, addr_t{0});
			append(unit, OpCode::EQ);
			ifelse(unit, [&]() {
				append(unit, OpCode::BLOAD, word_t{"L"},
				       addr_t{0}); 
				append(unit, OpCode::BFLUSH);
				append(unit, OpCode::CALLLBL, pgm.addr);
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

	/* Return to caller */
	append(unit, OpCode::BFLUSH, OpCode::LRET);

	/* Update unresolved calls */
	for (auto const & call : meta.calls) {
		for (auto const & pgm : meta.pgms) {
			if (pgm.name == call.name) {
				update(unit, call.addr, pgm.addr);
				break;
			}
		}
	}
};

} /* namespace */

void ncmacro::m700v::parse(Unit & unit, FileNames const & files) {
	AddrMeta meta;
	for (auto const & file : files) {
		std::ifstream in{file};
		Lexer lexer{in};
		Node_ptr root;
		addr_t pgmstart = unit.code.size();
		meta.pgms.push_back({file, pgmstart});
		unit.strings.push_back(file);

		if (make<pgm>(root, lexer)) {
			root->emit(meta, unit);
		}
	}

	finalise(meta, unit);
}
