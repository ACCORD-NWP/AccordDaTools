cmake_minimum_required(VERSION 3.18)
project(readgsa LANGUAGES C Fortran)

find_package(Python 3.10 REQUIRED
    COMPONENTS Interpreter Development.Module NumPy
)




message(STATUS "Python executable  : ${Python_EXECUTABLE}")
message(STATUS "NumPy include dirs : ${Python_NumPy_INCLUDE_DIRS}")

message("------------------------listinig ----------------------")
execute_process(
  COMMAND ls ${F2PY_INCLUDE_DIR}
)


message("------------------------listinig ----------------------")
execute_process(
    COMMAND "${Python_EXECUTABLE}"
            -c "import numpy.f2py; print(numpy.f2py.get_include())"
    OUTPUT_VARIABLE F2PY_INCLUDE_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

message(STATUS "F2PY_INCLUDE_DIR = ${F2PY_INCLUDE_DIR}")


set(f2py_module_name readgsa)
set(fortran_src_file ${CMAKE_SOURCE_DIR}/src/readgsa.F90)
set(f2py_module_c ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}module.c)

# Generate wrapper
add_custom_command(
    OUTPUT ${f2py_module_c}
    COMMAND ${Python_EXECUTABLE} -m numpy.f2py
            ${fortran_src_file}
            -m ${f2py_module_name}
            --lower
    DEPENDS ${fortran_src_file}
    COMMENT "Generating f2py wrapper"
)

#    Python_add_library( readgsa  MODULE WITH_SOABI
#    ${f2py_module_c} # Generated
#    "${F2PY_INCLUDE_DIR}/fortranobject.c" # From NumPy
#    "readgsa-f2pywrappers2.F90" # Fortran source(s) #"foo-f2pywrappers2.f90"
#    )

# Build module
Python_add_library(${f2py_module_name} MODULE WITH_SOABI
  ${F2PY_INCLUDE_DIR} 
    ${f2py_module_c}
    ${fortran_src_file}
)

target_include_directories(${f2py_module_name} PRIVATE
    ${F2PY_INCLUDE_DIR}
    ${Python_NumPy_INCLUDE_DIRS}
)

target_link_libraries(${f2py_module_name} PRIVATE
    Python::Module
    Python::NumPy
)
