STACK    =    stack

NAME    =    calculator-exe

BINNAME =  funEvalExpr


PATHEXE    =    $(shell $(STACK) path --local-install-root)/bin/$(NAME)

all:
	@$(STACK) build
	cp -f $(PATHEXE) ../$(BINNAME)
	ls

clean:
	rm -f *.o
	rm -f *.hi
	rm -rf .stack-work

fclean: clean
	rm -f ../$(BINNAME)

re: fclean all

.PHONY: all clean fclean re