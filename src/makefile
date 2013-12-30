C = icc
OPTS = -fpic -g -03 -Wall -c -lm
LOADER = gcc

OBJECTS = SW_Main.o SW_VegEstab.o SW_Control.o generic.o \
          rands.o Times.o mymemory.o filefuncs.o \
          SW_Files.o SW_Model.o SW_Site.o SW_SoilWater.o \
          SW_Markov.o SW_Weather.o SW_Sky.o SW_Output.o \
          SW_VegProd.o SW_Flow_lib.o SW_Flow.o SW_R_lib.o
all:
	R CMD SHLIB -o Rsoilwat.so $(OBJECTS)
.PHONY : clean
clean:
	rm *.o
	rm *.so
