#
# $Id$
# $Log$
#
#

############# MACROS ##############
.SECONDEXPANSION:
INTEL=no

#PROFILE=-pg

SYSTEM=UNIX
ARCH=-m32

FFF=$(FLUKA)/flutil/fff
#LINKER=lfluka

LINKER=ldpm3qmd
LFLUKA=$(FLUKA)/flutil/$(LINKER)

# If it is supplied as environmental variable
# $ make DEBUG=yes
DEBUG := yes

ifeq ($(DEBUG),yes)
	# do nothing
else
	DEBUG=no
endif

ifeq ($(INTEL),yes)
	CPP=icc
	CC=icc
else
	CPP=c++
	CC=cc
endif

WARNINGS=-Wall -Wshadow -Wpointer-arith -Wfloat-equal \
	-Wcast-align -Winline -Wredundant-decls

# _DEBUG can be 0, 1 or 2
# NDEBUG is used by assert()

ifeq ($(DEBUG),yes)
	# With debug info.
	ifeq ($(INTEL),yes)
		CFLAGS=-g $(PROFILE) $(INCDIR) $(WARNINGS) -D$(SYSTEM) \
			-D_DEBUG=2 -D_REENTRANT
	else
		CFLAGS=-g $(ARCH) $(PROFILE) $(INCDIR) $(WARNINGS) -D$(SYSTEM) \
			-D_DEBUG=2 -D_REENTRANT
	endif
else
	# Optimised version
	ifeq ($(INTEL),yes)
		#CFLAGS=-O2 $(INCDIR) -march=pentiumiii -openmp -pc32 -tpp6 -xK -Ob2 -D_DEBUG=0
		CFLAGS=-O2 $(INCDIR) -march=pentiumiii -D$(SYSTEM) \
			-DNDEBUG -D_DEBUG=0 -D_REENTRANT
	else
		CFLAGS=-O $(ARCH) $(INCDIR) $(WARNINGS) -D$(SYSTEM) \
			-DNDEBUG -D_DEBUG=0 -D_REENTRANT
	endif
endif

#LIBPTHREAD=pthread

#LIBBUTILS=butils

# library to be checked
ifeq ($(LINKER),lfluka)
	FLUKALIBS=$(FLUKA)/libflukahp.a
else
	FLUKALIBS=$(FLUKA)/libdpmmvax.a $(FLUKA)/librqmdmvax.a
endif

############# RULES ###############
.cc.o:
	$(CPP) -c $(CFLAGS) $<

.c.o:
	$(CC) -c $(CFLAGS) $<

.f.o:
	$(FFF) $<

############# TARGETS #############

.PHONY: all makeWcheck clean

default: all

all: clean makeWcheck

makeWcheck: $(FLUKALIBS)
	make flukaserver

flukaserver: source.o fluscw.o mdstck.o stuprf.o libFlukaIO.a \
	prstrf.o sttini.o sttupd.o sttfin.o sttdmp.o \
	ctrini.o ctrprs.o ctrlsc.o ctrsvc.o ctrchk.o ctrcnt.o ctrpsa.o ctrpss.o ctrupa.o ctrups.o \
	aryget.o aryset.o arysum.o aryprt.o \
	mtxget.o mtxset.o mtxrog.o mtxprt.o mtxinn.o mtxinu.o mtxina.o mtxmar.o mtxdet.o mtxinv.o mtxtrp.o mtxcom.o mtxmum.o mgdraw.o
	$(LFLUKA) -m fluka -o $@ $^

clean:
	rm -f flukaserver *.o *.map *.FOR *~
