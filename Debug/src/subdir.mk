################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/SW_Control.c \
../src/SW_Files.c \
../src/SW_Flow.c \
../src/SW_Flow_lib.c \
../src/SW_Main.c \
../src/SW_Markov.c \
../src/SW_Model.c \
../src/SW_Output.c \
../src/SW_R_lib.c \
../src/SW_Site.c \
../src/SW_Sky.c \
../src/SW_SoilWater.c \
../src/SW_VegEstab.c \
../src/SW_VegProd.c \
../src/SW_Weather.c \
../src/Times.c \
../src/filefuncs.c \
../src/generic.c \
../src/mymemory.c \
../src/rands.c 

OBJS += \
./src/SW_Control.o \
./src/SW_Files.o \
./src/SW_Flow.o \
./src/SW_Flow_lib.o \
./src/SW_Main.o \
./src/SW_Markov.o \
./src/SW_Model.o \
./src/SW_Output.o \
./src/SW_R_lib.o \
./src/SW_Site.o \
./src/SW_Sky.o \
./src/SW_SoilWater.o \
./src/SW_VegEstab.o \
./src/SW_VegProd.o \
./src/SW_Weather.o \
./src/Times.o \
./src/filefuncs.o \
./src/generic.o \
./src/mymemory.o \
./src/rands.o 

C_DEPS += \
./src/SW_Control.d \
./src/SW_Files.d \
./src/SW_Flow.d \
./src/SW_Flow_lib.d \
./src/SW_Main.d \
./src/SW_Markov.d \
./src/SW_Model.d \
./src/SW_Output.d \
./src/SW_R_lib.d \
./src/SW_Site.d \
./src/SW_Sky.d \
./src/SW_SoilWater.d \
./src/SW_VegEstab.d \
./src/SW_VegProd.d \
./src/SW_Weather.d \
./src/Times.d \
./src/filefuncs.d \
./src/generic.d \
./src/mymemory.d \
./src/rands.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross GCC Compiler'
	gcc -I/usr/share/R/include -I/usr/share/R/include/R_ext -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


