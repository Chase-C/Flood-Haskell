CC		= ghc -O2 -Wall
SRCDIR	= src
OBJDIR	= obj
FILES	= Main.hs Game.hs Entity.hs Utils.hs
SOURCES	= $(FILES:%.cpp=${SRCDIR}/%.cpp)
OBJECTS	= $(FILES:%.cpp=${OBJDIR}/%.cpp)
EXEC	= FishSim

all : ${EXEC}

${EXEC} : ${SOURCES}
	${CC} $^
