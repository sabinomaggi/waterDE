OPTIM = yes
DEBUG = no
WARN  = no

OSXMIN = 10.8

# Check platform (Darwin, Linux or Windows) and architecture (32/64 bit)
PLATFORM := $(shell uname)
ARCH := $(shell uname -m)

# Manually choose this platform for cross-compilation of Windows executable under Linux
### PLATFORM = MINGW

# Set fortran compiler according to platform
ifeq ($(PLATFORM), Darwin)
	export FC = gfortran
endif
ifeq ($(PLATFORM), Linux)
	export FC = gfortran
endif
ifeq ($(PLATFORM), MINGW)
	export FC = i686-w64-mingw32-gfortran
endif
ifeq ($(findstring NT-5.1,$(PLATFORM)), NT-5.1)
	export FC = gfortran
endif
### ifeq ($(PLATFORM), ADD_YOUR_PLATFORM_HERE)
### 	export FC = ADD_YOUR_COMPILER_HERE
### endif

# Compilation requires gfortran >= 4.4
# (http://gcc.gnu.org/ml/fortran/2008-09/msg00022.html)
ifeq ($(FC), gfortran)
	FCFLAGS += -DGFORT44
endif

# Set optimization/debug flags
ifeq ($(OPTIM), yes)
	FCFLAGS += -O2
endif
ifeq ($(DEBUG), yes)
	FCFLAGS += -g -O0
	LDFLAGS += -g
endif
ifeq ($(WARN), yes)
	FCFLAGS += -Wall
endif


# Set additional flags
FCFLAGS += -std=gnu -fbounds-check -x f95-cpp-input
LDFLAGS += -fbounds-check
LIBS = -lm


# Define includes and libraries
ifeq ($(PLATFORM), Darwin)
	BREW := $(shell brew --prefix)
	FCFLAGS += -m64 -DMACOSX
	LDFLAGS += -mmacosx-version-min=$(OSXMIN)
	INCLUDES = -I$(BREW)/include -L$(BREW)/lib
endif
ifeq ($(PLATFORM), Linux)
	ifeq ($(ARCH), x86_64)
		LNXENV = 64
	else
		LNXENV = 32
	endif
	FCFLAGS += -m$(LNXENV) -DLINUX
	LDFLAGS += -m$(LNXENV) -static
	INCLUDES = -I/usr/include -L/usr/lib
endif
ifeq ($(PLATFORM), MINGW)
	FCFLAGS += -m32 -DWINDOWS
	LDFLAGS += -m32 -static
	INCLUDES = -I/usr/include -L/usr/lib
endif
ifeq ($(findstring NT-5.1,$(PLATFORM)), NT-5.1)
	FCFLAGS += -m32 -DWINDOWS
	LDFLAGS += -m32 -static
	INCLUDES = -I/usr/include -L/usr/lib
endif
### ifeq ($(PLATFORM), ADD_YOUR_PLATFORM_HERE)
### 	FCFLAGS += -m32 -DLINUX
### 	LDFLAGS += -m32 -static
### 	INCLUDES = -I/usr/include -L/usr/lib
### endif



# List of source files
SRCS = data_modules.f90 \
       util_modules.f90 \
       getopt.f90       \
       calc_common.f90  \
       data_io.f90      \
       de.f90           \
       model_ftns.f90   \
       DE_Fortran90.f90 \
       waterDE.f90

EXE = waterDE

.SUFFIXES:
.SUFFIXES: .f90 .o
%.o : %.mod

### Do not edit below this line ###############################################

OBJS = $(SRCS:.f90=.o)

.f90.o:
	$(FC) -c $(FCFLAGS) $(INCLUDES) -o $@ $<

all: $(EXE)
	 rm -f *.log *.html *.out

$(EXE): $(OBJS)
	$(FC) $(LDFLAGS) $(INCLUDES) $(LIBS) -o $@ $(OBJS)

install:
	cp -p waterDE /usr/local/bin

.PHONY : clean
clean:
	rm -f *.o *.mod fort.* *~

fresh:
	rm -f *.o *.mod fort.* *.log *.html *.out *.png *~ waterDE

print-% :
	@echo $* = $($*)
