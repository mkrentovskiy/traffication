RM = /bin/rm

NAME = nca
OPTIONS	= -std=gnu99 -Wall -g -Os
LIBS = -lpcap

CFLAGS += -I./include $(OPTIONS)

MODULES   := . base inputs os proto sys 
SRC_DIR   := $(addprefix src/,$(MODULES))
SRC       := $(foreach sdir,$(SRC_DIR),$(wildcard $(sdir)/*.c))
OBJ       := $(patsubst src/%.c,src/%.o,$(SRC))

all:  $(NAME)

$(NAME): $(OBJ)
	$(CC) $(CFLAGS) -o $(NAME) $(OBJ) $(LIBS)

clean:
	$(RM) -rf $(NAME) $(OBJ) *.o *.a