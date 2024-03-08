cmake_minimum_required(VERSION 3.5)

project(readgsa LANGUAGES Fortran)

find_package (Python3 COMPONENTS Interpreter)

enable_testing()

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -fimplicit-none -fconvert=big-endian")
    set(bounds "-fbounds-check")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-free -implicitnone -convert big_endian")
    set(bounds "-check bounds")
endif()

add_subdirectory ( src  )



