cmake_minimum_required(VERSION 3.18) # Needed to avoid requiring embedded Python libs too

#project(readgsa
#  VERSION 1.0
#  DESCRIPTION "read GSA FILE module"
#  LANGUAGES C Fortran
#)

# Safety net
if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there.\n"
  )
endif()

# Grab Python, 3.8 or newer
find_package(Python 3.6 REQUIRED  COMPONENTS Interpreter Development.Module NumPy)

# Grab the variables from a local Python installation
# F2PY headers
execute_process(
  COMMAND "${Python_EXECUTABLE}"
  -c "import numpy.f2py; print(numpy.f2py.get_include())"
  OUTPUT_VARIABLE F2PY_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# GET THE MODULE SUFFIX 
execute_process( COMMAND "${Python_EXECUTABLE}" -c  "import importlib.machinery as imp ; print( imp.all_suffixes()[2])" OUTPUT_VARIABLE Module_SUFFIX)

# Print out the discovered paths
include(CMakePrintHelpers)
cmake_print_variables(Python_INCLUDE_DIRS)
cmake_print_variables(F2PY_INCLUDE_DIR)
cmake_print_variables(Python_NumPy_INCLUDE_DIRS)

# Common variables
set(f2py_module_name "readgsa")
set(fortran_src_file "${CMAKE_SOURCE_DIR}/src/readgsa.F90")
set(f2py_module_c "${f2py_module_name}module.c")
set(generated_module_file ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}${PYTHON_EXTENSION_MODULE_SUFFIX})


# Generate sources
add_custom_target(
  genpyf
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}"
)

# GENERATE THE C FILE WRAPPER 
add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}"
  COMMAND ${Python_EXECUTABLE}  -m "numpy.f2py"
                   "${fortran_src_file}"
                   -m "readgsa"
                   --lower # Important
		   DEPENDS readgsa.F90 # Fortran source
)

add_compile_options("-Wno-cpp")


# Set up target
Python_add_library(${CMAKE_PROJECT_NAME} MODULE WITH_SOABI
  "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}" # Generated
  "${F2PY_INCLUDE_DIR}/fortranobject.c"          # From NumPy
  "${fortran_src_file}"                          # Fortran source(s)
)




set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module)
#install(MODULE  ${CMAKE_BINARY_DIR}/src/${f2py_module_name}${Module_SUFFIX} RUNTIME  DESTINATION ${CMAKE_Fortran_MODULE_DIRECTORY})
#install(PROGRAMS ${CMAKE_BINARY_DIR}/bin/use_odb.sh DESTINATION bin)


# Depend on sources
target_link_libraries(${CMAKE_PROJECT_NAME} PRIVATE Python::NumPy)
add_dependencies(${CMAKE_PROJECT_NAME} genpyf)
target_include_directories(${CMAKE_PROJECT_NAME} PRIVATE "${F2PY_INCLUDE_DIR}")

#add_library ( SHARED ${f2py_module_name}${Module_SUFFIX}  )

