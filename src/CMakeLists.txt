# CmakeFile to compile 
cmake_minimum_required (VERSION 3.18)
project  (readgsa LANGUAGES C Fortran)

# Find system python
find_package(Python REQUIRED COMPONENTS Interpreter)

message(STATUS "System python: ${Python_EXECUTABLE}")

# Create venv inside build tree
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

find_package(Python REQUIRED COMPONENTS Interpreter Development NumPy)
message(STATUS "Python from venv: ${Python_EXECUTABLE}")

# Locate f2py include
execute_process(
    COMMAND ${Python_EXECUTABLE}
            -c "import numpy.f2py;print(numpy.f2py.get_include())"
    OUTPUT_VARIABLE F2PY_INCLUDE
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Generate f2py wrapper
set(f2py_module_name readgsa)
set(FSRC        ${CMAKE_SOURCE_DIR}/src/readgsa.F90)


add_custom_command(
    OUTPUT  readgsa 
    COMMAND ${Python_EXECUTABLE} -m numpy.f2py
            ${FSRC}
            -m ${f2py_module_name}
            --lower
    DEPENDS ${FSRC} )

# Build the module
Python_add_library(${f2py_module_name}   MODULE WITH_SOABI
    ${F2PY_INCLUDE_DIR}
    ${f2py_module_name}
    ${FSRC}
)

# Include 
target_include_directories(${f2py_module_name} PRIVATE
    ${F2PY_INCLUDE_DIR}
    ${Python_NumPy_INCLUDE_DIRS}
)

# Libs 
target_link_libraries(${f2py_module_name} PRIVATE
    Python::Module
    Python::NumPy
)

# Set the suffix to  " .so " 
set_target_properties(readgsa PROPERTIES
    PREFIX ""
    SUFFIX ".so"
)

# Add it to the python modules 
install(TARGETS readgsa LIBRARY DESTINATION ${CMAKE_SOURCE_DIR}/modules )
