package sexpr

import (
	"errors"
	//"math/big" // You will need to use this package in your implementation.
	"math/big"
)

// ErrEval is the error value returned by the Evaluator if the contains
// an invalid token.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrEval = errors.New("eval error")

func (expr *SExpr) Eval() (*SExpr, error) {
	if expr == nil {
		return nil, ErrEval
	}

	if expr.isNil() {
		return mkNil(), nil
	}

	if expr.isNumber() {
		return expr, nil
	}

	if isSymbol(expr, "T") {
		return expr, nil
	}

	if expr.isAtom() {
		return nil, ErrEval
	}

	if !expr.car.isSymbol() {
		return nil, ErrEval
	}

	if isSymbol(expr.car, "QUOTE") {
		return evalQuote(expr.cdr)
	}
	if isSymbol(expr.car, "CAR") {
		return evalCar(expr.cdr)
	}
	if isSymbol(expr.car, "CDR") {
		return evalCdr(expr.cdr)
	}
	if isSymbol(expr.car, "CONS") {
		return evalCons(expr.cdr)
	}
	if isSymbol(expr.car, "LENGTH") {
		return evalLength(expr.cdr)
	}
	if isSymbol(expr.car, "ATOM") {
		return evalAtom(expr.cdr)
	}
	if isSymbol(expr.car, "LISTP") {
		return evalListp(expr.cdr)
	}
	if isSymbol(expr.car, "ZEROP") {
		return evalZerop(expr.cdr)
	}
	if isSymbol(expr.car, "+") {
		return evalAddArgs(expr.cdr, big.NewInt(0))
	}
	if isSymbol(expr.car, "*") {
		return evalMulArgs(expr.cdr, big.NewInt(1))
	}

	return nil, ErrEval
}

func isSymbol(expr *SExpr, name string) bool {
	return expr != nil && expr.isSymbol() && expr.atom.literal == name
}

func boolSExpr(value bool) *SExpr {
	if value {
		return mkSymbolTrue()
	}
	return mkNil()
}

func oneArg(args *SExpr) (*SExpr, error) {
	if args == nil || args.isNil() || !args.isConsCell() {
		return nil, ErrEval
	}
	if !args.cdr.isNil() {
		return nil, ErrEval
	}
	return args.car, nil
}

func twoArgs(args *SExpr) (*SExpr, *SExpr, error) {
	if args == nil || args.isNil() || !args.isConsCell() {
		return nil, nil, ErrEval
	}
	if args.cdr == nil || args.cdr.isNil() || !args.cdr.isConsCell() {
		return nil, nil, ErrEval
	}
	if !args.cdr.cdr.isNil() {
		return nil, nil, ErrEval
	}
	return args.car, args.cdr.car, nil
}

func evalQuote(args *SExpr) (*SExpr, error) {
	arg, err := oneArg(args)
	if err != nil {
		return nil, ErrEval
	}
	return arg, nil
}

func evalOneArg(args *SExpr) (*SExpr, error) {
	arg, err := oneArg(args)
	if err != nil {
		return nil, ErrEval
	}
	value, err := arg.Eval()
	if err != nil {
		return nil, ErrEval
	}
	return value, nil
}

func evalCar(args *SExpr) (*SExpr, error) {
	value, err := evalOneArg(args)
	if err != nil {
		return nil, ErrEval
	}
	if value.isNil() {
		return mkNil(), nil
	}
	if !value.isConsCell() {
		return nil, ErrEval
	}
	return value.car, nil
}

func evalCdr(args *SExpr) (*SExpr, error) {
	value, err := evalOneArg(args)
	if err != nil {
		return nil, ErrEval
	}
	if value.isNil() {
		return mkNil(), nil
	}
	if !value.isConsCell() {
		return nil, ErrEval
	}
	return value.cdr, nil
}

func evalCons(args *SExpr) (*SExpr, error) {
	firstArg, secondArg, err := twoArgs(args)
	if err != nil {
		return nil, ErrEval
	}

	firstValue, err := firstArg.Eval()
	if err != nil {
		return nil, ErrEval
	}

	secondValue, err := secondArg.Eval()
	if err != nil {
		return nil, ErrEval
	}

	return mkConsCell(firstValue, secondValue), nil
}

func evalLength(args *SExpr) (*SExpr, error) {
	value, err := evalOneArg(args)
	if err != nil {
		return nil, ErrEval
	}

	length, err := properLength(value)
	if err != nil {
		return nil, ErrEval
	}

	return mkNumber(length), nil
}

func properLength(expr *SExpr) (*big.Int, error) {
	if expr == nil {
		return nil, ErrEval
	}

	if expr.isNil() {
		return big.NewInt(0), nil
	}

	if !expr.isConsCell() {
		return nil, ErrEval
	}

	restLength, err := properLength(expr.cdr)
	if err != nil {
		return nil, ErrEval
	}

	return new(big.Int).Add(big.NewInt(1), restLength), nil
}

func evalAtom(args *SExpr) (*SExpr, error) {
	value, err := evalOneArg(args)
	if err != nil {
		return nil, ErrEval
	}

	return boolSExpr(value.isAtom()), nil
}

func evalListp(args *SExpr) (*SExpr, error) {
	value, err := evalOneArg(args)
	if err != nil {
		return nil, ErrEval
	}

	return boolSExpr(value.isConsCell()), nil
}

func evalZerop(args *SExpr) (*SExpr, error) {
	value, err := evalOneArg(args)
	if err != nil {
		return nil, ErrEval
	}

	if !value.isNumber() {
		return nil, ErrEval
	}

	return boolSExpr(value.atom.num.Sign() == 0), nil
}

func evalAddArgs(args *SExpr, sum *big.Int) (*SExpr, error) {
	if args == nil {
		return nil, ErrEval
	}

	if args.isNil() {
		return mkNumber(sum), nil
	}

	if !args.isConsCell() {
		return nil, ErrEval
	}

	value, err := args.car.Eval()
	if err != nil {
		return nil, ErrEval
	}

	if !value.isNumber() {
		return nil, ErrEval
	}

	newSum := new(big.Int).Add(sum, value.atom.num)
	return evalAddArgs(args.cdr, newSum)
}

func evalMulArgs(args *SExpr, product *big.Int) (*SExpr, error) {
	if args == nil {
		return nil, ErrEval
	}

	if args.isNil() {
		return mkNumber(product), nil
	}

	if !args.isConsCell() {
		return nil, ErrEval
	}

	value, err := args.car.Eval()
	if err != nil {
		return nil, ErrEval
	}

	if !value.isNumber() {
		return nil, ErrEval
	}

	newProduct := new(big.Int).Mul(product, value.atom.num)
	return evalMulArgs(args.cdr, newProduct)
}