CC=g++
CPPFLAGS=-Wall -std=c++11 -O2
LDLIBS=-lm -L/usr/local/lib -lmsgpack

EXES= aoj-problem-spherical-k-means

all: $(EXES)
all: CPPFLAGS+=-DDEBUG=0
debug: $(EXES)
debug: CPPFLAGS+=-DDEBUG -g

SRCS_SPHERICAL=\
aoj-problem-spherical-k-means.cpp \
data-io.cpp \

OBJS_SPHERICAL= $(SRCS_SPHERICAL:.cpp=.o)
aoj-problem-spherical-k-means: $(OBJS_SPHERICAL)

# Assemble dependency. see: http://stackoverflow.com/a/2394651
depend: .depend
.depend: $(SRCS_SPHERICAL)
	rm -f ./.depend
	$(CC) $(CPPFLAGS) -MM $^ > ./.depend;
include .depend

clean:
	rm -f $(EXES) $(OBJS_SPHERICAL)

