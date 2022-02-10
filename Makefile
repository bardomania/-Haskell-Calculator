all:
	@make -C calculator/

clean:
	@make clean -C calculator/

fclean:
	@make fclean -C calculator/

re: fclean all

.PHONY: clean re fclean all