package sexpr

import "errors"

// ErrParser is the error value returned by the Parser if the string is not a
// valid term.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrParser = errors.New("parser error")

//
// <sexpr>       ::= <atom> | <pars> | QUOTE <sexpr>
// <atom>        ::= NUMBER | SYMBOL
// <pars>        ::= LPAR <dotted_list> RPAR | LPAR <proper_list> RPAR
// <dotted_list> ::= <proper_list> <sexpr> DOT <sexpr>
// <proper_list> ::= <sexpr> <proper_list> | \epsilon
//
type Parser interface {
	Parse(string) (*SExpr, error)
}

type parserImpl struct {
	lex *lexer
	tok *token
}

func NewParser() Parser {
	return &parserImpl{}
}

func (p *parserImpl) readNext() error {
	tok, err := p.lex.next()
	if err != nil {
		return ErrParser
	}
	p.tok = tok
	return nil
}

func (p *parserImpl) Parse(input string) (*SExpr, error) {
	p.lex = newLexer(input)
	if p.readNext() != nil {
		return nil, ErrParser
	}

	expr, err := p.parseSExpr()
	if err != nil {
		return nil, ErrParser
	}

	if p.tok.typ != tokenEOF {
		return nil, ErrParser
	}
	return expr, nil
}

func (p *parserImpl) parseSExpr() (*SExpr, error) {
	if p.tok.typ == tokenNumber {
		expr := mkAtom(p.tok)
		if p.readNext() != nil {
			return nil, ErrParser
		}
		return expr, nil
	}

	if p.tok.typ == tokenSymbol {
		if p.tok.literal == "NIL" {
			if p.readNext() != nil {
				return nil, ErrParser
			}
			return mkNil(), nil
		}
		expr := mkAtom(p.tok)
		if p.readNext() != nil {
			return nil, ErrParser
		}
		return expr, nil
	}

	if p.tok.typ == tokenQuote {
		if p.readNext() != nil {
			return nil, ErrParser
		}
		expr, err := p.parseSExpr()
		if err != nil {
			return nil, ErrParser
		}
		return mkConsCell(mkSymbol("QUOTE"), mkConsCell(expr, mkNil())), nil
	}

	if p.tok.typ == tokenLpar {
		if p.readNext() != nil {
			return nil, ErrParser
		}
		return p.parseList()
	}

	return nil, ErrParser
}

func (p *parserImpl) parseList() (*SExpr, error) {
	if p.tok.typ == tokenRpar {
		if p.readNext() != nil {
			return nil, ErrParser
		}
		return mkNil(), nil
	}

	first, err := p.parseSExpr()
	if err != nil {
		return nil, ErrParser
	}

	rest, err := p.parseListTail()
	if err != nil {
		return nil, ErrParser
	}

	return mkConsCell(first, rest), nil
}

func (p *parserImpl) parseListTail() (*SExpr, error) {
	if p.tok.typ == tokenRpar {
		if p.readNext() != nil {
			return nil, ErrParser
		}
		return mkNil(), nil
	}

	if p.tok.typ == tokenDot {
		if p.readNext() != nil {
			return nil, ErrParser
		}
		rest, err := p.parseSExpr()
		if err != nil {
			return nil, ErrParser
		}
		if p.tok.typ != tokenRpar {
			return nil, ErrParser
		}
		if p.readNext() != nil {
			return nil, ErrParser
		}
		return rest, nil
	}

	next, err := p.parseSExpr()
	if err != nil {
		return nil, ErrParser
	}

	rest, err := p.parseListTail()
	if err != nil {
		return nil, ErrParser
	}

	return mkConsCell(next, rest), nil
}