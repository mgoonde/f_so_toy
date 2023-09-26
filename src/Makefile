F90 := gfortran

LIB := libthemodule.so

FFLAGS := -fPIC
FDEBUG :=

FLAGS := $(FFLAGS) $(FDEBUG)

SRC := ./
OBJ := $(SRC)/Obj
LIBD := $(SRC)/../lib

MOD := -I$(OBJ) -J$(OBJ)



header := \
	$(SRC)/lone_routine.f90 \
	$(SRC)/m_tools.f90 \
	$(SRC)/themodule_h.f90 \
	$(SRC)/thing_datatype.f90
header.o := $(header:$(SRC)/%.f90=$(OBJ)/%.o)

routine := $(sort $(filter-out %_h.f90, $(wildcard $(SRC)/*.f90)))
routine.o := $(routine:$(SRC)/%.f90=$(OBJ)/%.o)

api := \
	$(SRC)/api.f90
api.o := $(api:$(SRC)/%.f90=$(OBJ)/%.o)


default: lib

lib: $(OBJ) $(LIBD) $(LIB)

$(OBJ):
	@if [ ! -d $(OBJ) ]; then mkdir $(OBJ); fi
$(LIBD):
	@if [ ! -d $(LIBD) ]; then mkdir $(LIBD); fi

$(LIB): $(header.o) $(routine.o) $(api.o)
	@echo $^
	$(F90) -o $(LIBD)/$@ -shared -fPIC $^


$(OBJ)/%.o: $(SRC)/%.f90
	$(F90) $(MOD) -c $(FLAGS)  $< -o $@


clean:
	rm -rf $(OBJ) $(LIBD) $(LIB)
