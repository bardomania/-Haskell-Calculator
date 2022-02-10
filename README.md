# calculator

## Warnings

Make sure to have installed ghc, stack cabal, etc...

## To compile

make

## Result

Binary named 'funEvalExpr' which is a calculator
The implemented operator are: -+*/^()

```
> ./funEvalExpr "(5+(4*3))"
17.00

> ./funEvalExpr "5"        
5

[...]

> ./funEvalExpr "5^5*(1*3)"
9375.00
```