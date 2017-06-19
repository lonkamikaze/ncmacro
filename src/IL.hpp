/** \file
 * Provides an intermediate language that can be generated from an
 * NC code parser.
 */

#ifndef _NCMACRO_IL_HPP_
#define _NCMACRO_IL_HPP_

#include "Callback.hpp"

#include <cstdint>
#include <ostream>
#include <functional>
#include <unordered_map>

namespace ncmacro {
namespace il {

/**
 *
 * | Opcode  | Arg        | Push | Pop  |
 * |---------|------------|------|------|
 * | NOP     | -          | -    | -    |
 * | LOAD    | value      | 1    | -    |
 * | LOAD0   | -          | 1    | -    |
 * | LOAD1   | -          | 1    | -    |
 * | GLOAD   | addr       | 1    | -    |
 * | LLOAD   | addr       | 1    | -    |
 * | CLONE   | addr       | #1   | -    |
 * | GSTORE  | addr       | -    | 1    |
 * | LSTORE  | addr       | -    | 1    |
 * | POP     | addr       | -    | #1   |
 * | FLIP    | addr       | #1   | #1   |
 * | CALL    | addr       | 1    | -    |
 * | RET     | -          | -    | 1    |
 * | LCALL   | addr       | 1    | -    |
 * | LRET    | -          | -    | 1    |
 * | GOTO    | addr       | -    | -    |
 * | EXIT    | -          | -    | -    |
 * | LBL     | addr       | -    | -    |
 * | CALLLBL | addr       | 1    | 1    |
 * | GOTOLBL | -          | -    | 1    |
 * | WRAPLBL | addr       | -    | -    |
 * | IF      | -          | -    | 1    |
 * | MUL     | -          | 1    | 2    |
 * | DIV     | -          | 1    | 2    |
 * | MOD     | -          | 1    | 2    |
 * | AND     | -          | 1    | 2    |
 * | ADD     | -          | 1    | 2    |
 * | SUB     | -          | 1    | 2    |
 * | OR      | -          | 1    | 2    |
 * | XOR     | -          | 1    | 2    |
 * | EQ      | -          | 1    | 2    |
 * | NE      | -          | 1    | 2    |
 * | GT      | -          | 1    | 2    |
 * | LT      | -          | 1    | 2    |
 * | GE      | -          | 1    | 2    |
 * | LE      | -          | 1    | 2    |
 * | EQI     | -          | 1    | 2    |
 * | NEI     | -          | 1    | 2    |
 * | NEG     | -          | 1    | 1    |
 * | NOT     | -          | 1    | 1    |
 * | FROMDEG | -          | 1    | 1    |
 * | TODEG   | -          | 1    | 1    |
 * | SIN     | -          | 1    | 1    |
 * | COS     | -          | 1    | 1    |
 * | TAN     | -          | 1    | 1    |
 * | ASIN    | -          | 1    | 1    |
 * | ATAN    | -          | 1    | 1    |
 * | ACOS    | -          | 1    | 1    |
 * | SQRT    | -          | 1    | 1    |
 * | ABS     | -          | 1    | 1    |
 * | FROMBCD | -          | 1    | 1    |
 * | TOBCD   | -          | 1    | 1    |
 * | ROUND   | -          | 1    | 1    |
 * | FLOOR   | -          | 1    | 1    |
 * | TRUNC   | -          | 1    | 1    |
 * | CEIL    | -          | 1    | 1    |
 * | PAD     | -          | 1    | 1    |
 * | LN      | -          | 1    | 1    |
 * | EXP     | -          | 1    | 1    |
 * | BCOMMIT | -          | -    | -    |
 * | BFLUSH  | -          | -    | -    |
 * | BAPPEND | word       | -    | 1    |
 * | BMATCH  | word,value | 1    | -    |
 * | BHAS    | word       | 1    | -    |
 * | BLOAD   | word,addr  | 1    | -    |
 * | LOADSTR | string     | 1    | -    |
 * | PRINT   | string     | -    | -    |
 * | WARN    | string     | -    | -    |
 * | ERROR   | string     | -    | -    |
 *
 */
enum class OpCode : uint8_t {
	NOP,     /**< Do nada. */
	LOAD,    /**< Push arg. */
	LOAD0,   /**< Push value 0. */
	LOAD1,   /**< Push value 1. */
	GLOAD,   /**< Push global value. */
	LLOAD,   /**< Push local value. */
	CLONE,   /**< Push arg values from stack top onto stack. */
	GSTORE,  /**< Pop value and store as a global. */
	LSTORE,  /**< Pop value and store as a local. */
	POP,     /**< Pop arg values from stack and discard. */
	FLIP,    /**< Revert order of arg values on stack. */
	CALL,    /**< Push registers on stack and goto arg. Creates
	          **< a new local variable context. */
	RET,     /**< Return to address on stack. Destroys the local
	          **< variable context. */
	LCALL,   /**< Local call, like CALL but stays in the same local
	          **< variable space. */
	LRET,    /**< Local return, like RET but stays in the same local
	          **< variable space. */
	GOTO,    /**< Set PC. */
	EXIT,    /**< Terminate execution. */
	LBL,     /**< Argument is a label for GOTOLBL. */
	CALLLBL, /**< CALL and GOTOLBL, expects an error string and
	          **< the label on top of the stack. */
	GOTOLBL, /**< Go to the next matching LBL. Expects an error
	          **< string and a LBL on to pof the stack. */
	WRAPLBL, /**< When reaching this during LBL search, start from arg. */
	IF,      /**< Execute next instruction if value is not 0. */
	MUL,     /**< Multiply. */
	DIV,     /**< Divide. */
	MOD,     /**< Modulus. */
	AND,     /**< Cast to addr_t and perform bitwise and. */
	ADD,     /**< Addition. */
	SUB,     /**< Subtraction. */
	OR,      /**< Cast to addr_t and perform bitwise or. */
	XOR,     /**< Cast to addr_t and perform bitwise xor. */
	EQ,      /**< Check for equality. */
	NE,      /**< Check for inequality. */
	GT,      /**< Greater than. */
	LT,      /**< Less than. */
	GE,      /**< Greater than or equal. */
	LE,      /**< Less than or equal. */
	EQI,     /**< Equally initialiased. */
	NEI,     /**< Not equally initialised. */
	NEG,     /**< Negate. */
	NOT,     /**< 0 => 1; !0 => 0. */
	FROMDEG, /**< Convert degree to radian. */
	TODEG,   /**< Convert radian to degree. */
	SIN,     /**< Trigonometric function. */
	COS,     /**< Trigonometric function. */
	TAN,     /**< Trigonometric function. */
	ASIN,    /**< Trigonometric function. */
	ATAN,    /**< Trigonometric function. */
	ACOS,    /**< Trigonometric function. */
	SQRT,    /**< Square root. */
	ABS,     /**< Absolute. */
	FROMBCD, /**< Convert bcd to binary (looses sign). */
	TOBCD,   /**< Convert binary to bcd (looses sign). */
	ROUND,   /**< Round to nearest. */
	FLOOR,   /**< Round towards left. */
	TRUNC,   /**< Round towards zero. */
	CEIL,    /**< Round towards right. */
	PAD,     /**< Round away from zero. */
	LN,      /**< Natural logarithm. */
	EXP,     /**< Exponential function. */
	BCOMMIT, /**< Callback with the block. */
	BFLUSH,  /**< Flush the block. */
	BAPPEND, /**< Append word with value from the stack to block. */
	BMATCH,  /**< Push the number of block words matching the
	          **< given word, value (arg0, arg1) pair. */
 	BHAS,    /**< Push the number of block words matching the
 	          **< given word (arg). */
	BLOAD,   /**< Push a value from the block words.
	          **< The value is selected by word (arg0) and index (arg1). */
	LOADSTR, /**< Push a string reference. */
	PRINT,   /**< Print a string. */
	WARN,    /**< Print a warning string. */
	ERROR,   /**< Print a string, terminate. */
	VERSION  /**< The number of op codes, used for versioning. */
};

using StrMap = std::unordered_map<string_t, addr_t>;
using Strings = std::vector<string_t>;
using Code = std::vector<char>;

/**
 * A translation unit containing strings and code.
 */
struct Unit {
	StrMap strmap;
	Strings strings;
	Code code;
};

template <typename T> void append(Unit &, T const);
void append(Unit &, string_t const &);

/**
 * The append family of functions adds code to the translation
 * unit.
 */
template <typename Head, typename... Tail>
void append(Unit & unit, Head head, Tail... tail) {
	append(unit, head);
	append(unit, tail...);
}

void update(Unit &, addr_t const, addr_t const);

void ifelse(Unit &, std::function<void()> const &);
void ifelse(Unit &, std::function<void()> const &,
            std::function<void()> const &);
void repeat(Unit &, std::function<void()> const &);

std::ostream & operator <<(std::ostream &, Unit const &);

void run(Unit const &, Callback &);

} /* namespace il */
} /* namespace ncmacro */

#endif /* _NCMACRO_IL_HPP_ */
