# CmakeFile to compile readgsa.F90  
# Creates a virtualenv , where f2py compiles the routine
# When the venv is created , the numpy module is installed.
# ==> Eveything is done inside the build directory  !
cmake_minimum_required (VERSION 3.18)

project  (readgsa LANGUAGES C Fortran)


# Find system python
find_package(Python REQUIRED COMPONENTS Interpreter)

message(STATUS "System python: ${Python_EXECUTABLE}")

# Create venv inside build 
set(VENV_DIR ${CMAKE_BINARY_DIR}/venv)
if(NOT EXISTS ${VENV_DIR}/bin/python)
    message(STATUS "Creating virtualenv")
    execute_process(
        COMMAND ${Python_EXECUTABLE} -m venv ${VENV_DIR}
        RESULT_VARIABLE venv_status
    )
    if(NOT venv_status EQUAL 0)
        message(FATAL_ERROR "Failed to create virtualenv")
    endif()
    execute_process(  COMMAND ${VENV_DIR}/bin/pip install --upgrade pip )
    execute_process(  COMMAND ${VENV_DIR}/bin/pip install numpy    )
endif()


# Switch python to venv
set(Python_EXECUTABLE ${VENV_DIR}/bin/python CACHE FILEPATH "" FORCE)

# Look for numpy inside venv 
find_package(Python REQUIRED COMPONENTS Interpreter Development NumPy)
message(STATUS "Python from venv: ${Python_EXECUTABLE}")

# Locate f2py includes
execute_process( COMMAND ${Python_EXECUTABLE} -c "import numpy.f2py;print(numpy.f2py.get_include())"    OUTPUT_VARIABLE F2PY_INCLUDE_DIR  OUTPUT_STRIP_TRAILING_WHITESPACE)

# Common variables
set(f2py_module_name   "readgsa")
set(fsrc_path          "${CMAKE_SOURCE_DIR}/src/readgsa.F90")
set(fortran_src_file   "${fsrc_path}"  )
set(f2py_module_c      "${f2py_module_name}module.c")

add_custom_command( OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}" COMMAND ${Python_EXECUTABLE}  -m "numpy.f2py" "${fortran_src_file}"
                   -m ${f2py_module_name}
                   --lower 
                   DEPENDS readgsa.F90 # Fortran source
)

# Set up target
Python_add_library( readgsa  MODULE WITH_SOABI
  "${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_c}" # Generated   .c  module 
  "${F2PY_INCLUDE_DIR}/fortranobject.c"          # From NumPy
  "${fortran_src_file}"                          # Fortran source
)

# Include 
target_include_directories(${f2py_module_name} PRIVATE    ${F2PY_INCLUDE_DIR}    ${Python_NumPy_INCLUDE_DIRS})

# Libs 
target_link_libraries(${f2py_module_name} PRIVATE    Python::Module    Python::NumPy)

# Set the suffix to  " .so "    without machine_system  architecture/realease info  ..etc   
set_target_properties(readgsa PROPERTIES    PREFIX ""    SUFFIX ".so")

# Add it to the python modules 
install(TARGETS readgsa LIBRARY DESTINATION ${CMAKE_SOURCE_DIR}/modules )
