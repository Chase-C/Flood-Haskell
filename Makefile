CC		= ghc -O0 -Wall
FLAGS	= -tmpdir /tmp
SRCDIR	= src
FILES	= Main.hs Types.hs Board.hs UI.hs Utils.hs
SOURCES	= $(FILES:%.hs=${SRCDIR}/%.hs)
EXEC	= Flood

all : ${EXEC}

${EXEC} : ${SOURCES}
	${CC} ${FLAGS} -o $@ $^
	- rm src/*.{hi,o}
