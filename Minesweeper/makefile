# Makefile for Fortran project with modules

# Compiler and flags
FC = gfortran
FFLAGS = -Wall -Wextra -std=f2008

# Target executable name
TARGET = main

# Automatically find all .f90 files in current directory
SOURCES = $(wildcard *.f90)

# Object files (automatically derived from sources)
OBJECTS = $(SOURCES:.f90=.o)

# Module files - automatically find all mod_*.mod files that will be created
# This assumes each .f90 file (except main.f90) creates a mod_<name>.mod file
MODULE_SOURCES = $(filter-out main.f90, $(SOURCES))
MODULES = $(patsubst %.f90,mod_%.mod,$(MODULE_SOURCES))

# Default target
all: $(TARGET)

# Link object files to create executable
$(TARGET): $(OBJECTS)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS)

# Compile source files to object files
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Automatic module dependencies - main.o depends on all other object files
# This ensures all modules are compiled before main
MODULE_OBJECTS = $(MODULE_SOURCES:.f90=.o)
main.o: $(MODULE_OBJECTS)
board.o: color.o

# Clean up generated files
clean:
	rm -f $(OBJECTS) $(MODULES) $(TARGET)

# Declare phony targets
.PHONY: all clean
