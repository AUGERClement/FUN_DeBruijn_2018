##
## EPITECH PROJECT, 2018
## deBruijn
## File description:
## Makefile
##

NAME	=	deBruijn

NAME_T	=	test_de_bruijn

SRC	=	app/Main.hs

TEST	=	test/Spec.hs

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .
	mv test-exe $(NAME)

$(NAME_T):	$(TEST)
	stack build --copy-bins --local-bin-path .
	mv test-exe $(NAME_T)



clean:
	stack clean
	rm .stack-work deBruijn.cabal -rf

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
