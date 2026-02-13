# Define directories containing source 
SHELL = /bin/sh

#FC = pgf90
FC = gfortran

# pgf90 flags:
#CFLAGS = -g -Mbounds

# gfortran flags:
CFLAGS = -Jexec

# xlf90 flags:
#CFLAGS = -g -C -qsuffix=f=F90

# Allowed SUFFIXES:
.SUFFIXES :  
.SUFFIXES :  .F90 .o .mod

SDIR=./src
XDIR=./exec

SOURCE = ${SDIR}/depstat_main.F90

OBJS  = ${XDIR}/parameters.o \
        ${XDIR}/array_definitions.o \
        ${XDIR}/depstat_main.o \
        ${XDIR}/prepare.o \
        ${XDIR}/vcoordinate.o \
        ${XDIR}/fetchOneTime.o \
        ${XDIR}/getSensorNumber.o \
        ${XDIR}/average.o \
        ${XDIR}/report.o

MODULES = ${XDIR}/parameters.o ${XDIR}/array_definitions.o


# Object dependencies:

${XDIR}/depstat_main : $(OBJS) $(MODULES) 
	$(FC) -o ${XDIR}/depstat_main $(OBJS)

${XDIR}/parameters.o : ${SDIR}/parameters.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/parameters.F90 -o ${XDIR}/parameters.o

${XDIR}/depstat_main.o : ${SDIR}/depstat_main.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/depstat_main.F90 -o ${XDIR}/depstat_main.o

${XDIR}/prepare.o : ${SDIR}/prepare.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/prepare.F90 -o ${XDIR}/prepare.o

${XDIR}/vcoordinate.o : ${SDIR}/vcoordinate.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/vcoordinate.F90 -o ${XDIR}/vcoordinate.o

${XDIR}/fetchOneTime.o : ${SDIR}/fetchOneTime.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/fetchOneTime.F90 -o ${XDIR}/fetchOneTime.o

${XDIR}/getSensorNumber.o : ${SDIR}/getSensorNumber.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/getSensorNumber.F90 -o ${XDIR}/getSensorNumber.o

${XDIR}/average.o : ${SDIR}/average.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/average.F90 -o ${XDIR}/average.o

${XDIR}/report.o : ${SDIR}/report.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/report.F90 -o ${XDIR}/report.o

${XDIR}/array_definitions.o : ${SDIR}/array_definitions.F90
	$(FC) -c $(CFLAGS) $(INCFLAG) ${SDIR}/array_definitions.F90 -o ${XDIR}/array_definitions.o

#

clean : 
	rm -f ${OBJS} ${XDIR}/*.mod ${SDIR}/*~
clobber : 
	rm -f  ${OBJS} ${XDIR}/*.mod ${SDIR}/*~ ${XDIR}/depstat_main 
