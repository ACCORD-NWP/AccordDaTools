from numpy import f2py
import os  
import sys  
import platform  

"""
Compile the fortran routine readgsa.F90  
and creates a python extension readgsa.so  
which  will be called as a module 

Tested with gfortran compiler and default compilation options 

DEPENDENCY :  python3.8 or heigher               !
              gfortran - GNU Fortran (GCC) 8.5.0 
           
"""

# CURRENT DIR 
basedir=os.getenv("PWD")

# SOME INPUTS 
_compiler   ="gfortran"
_target_name="readgsa"


# COMPILER 
_cpath=os.popen("which "+_compiler).read()
if os.path.exists (_cpath.rstrip()):
   print("Use fortran compiler :" , _cpath )
else:
   print("Unknown compiler '"+_compiler+"' or '"+_compiler +"'  not found !" )
   sys.exit()

# PYTHON 
_vmaj=sys.version_info.major
_vmin=sys.version_info.minor
py_ver=str(_vmaj)+str(_vmin)

# MACHINE ENV
_arch=platform.uname()
name=_arch.system.lower()
machine=_arch.machine.lower()

pyobject=_target_name+".cpython-*-"+machine+"-"+name+"-gnu.so"

# COMPILE 
if os.path.isfile (basedir+"/src/"+_target_name+".F90" ):
   with open(basedir+"/src/"+_target_name+".F90") as sourcefile:
         sourcecode = sourcefile.read()
else:
   print("MISSING SOURCE CODE readgsa.F90 UNDER :" , basedir+"/src/"+"\n" )  
   sys.exit()

# RETURN 0 IF SUCCESS 1 IF NOT !
icomp=f2py.compile(sourcecode, 
                  modulename=_target_name, 
                  extra_args="--f90exec="+_cpath,
                  extension='.F90',
                  verbose=True )

if icomp == 0:
   print( "     ")
   print( "Successfully compiled python extension ", _target_name+".so" )
   os.system( "mv "+pyobject + "   "+basedir+"/modules/"+_target_name+".so" )
else:
   print( "    ")
   print( "Failed to compile the extension " , _target_name+".so" )
   sys.exit()
