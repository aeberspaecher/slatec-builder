=================
libslatec builder
=================

About
=====

This dirty script helps to build SLATEC (http://www.netlib.org/slatec/), a
comprehensive mathematical library written in Fortran.

Dependencies
============

The ``install`` script has three dependencies. If they are missing at
runtime, the script will try to download them. The dependencies are:

 1. John Burkardt's F90 version of slatec, available from

    http://people.sc.fsu.edu/~jburkardt/f_src/slatec/slatec.html

    Direct link:
   
    http://people.sc.fsu.edu/~jburkardt/f_src/slatec/slatec.f90

 2. John Burkardt's tool f90split, available from

    http://people.sc.fsu.edu/~jburkardt/f_src/f90split/f90split.html

    Direct link:
   
    http://people.sc.fsu.edu/~jburkardt/f_src/f90split/f90split.f90

 3. The files ``d1mach.f90``, ``r1mach.f90``, ``i1mach.f90`` available from

    http://www.nsc.liu.se/~boein/ifip/kyoto/workshop-info/proceedings/einarsson/f90/

    These files use F90 routines to determine some machine parameters.

    Direct links:
   
    http://www.nsc.liu.se/~boein/ifip/kyoto/workshop-info/proceedings/einarsson/f90/d1mach.f90
    http://www.nsc.liu.se/~boein/ifip/kyoto/workshop-info/proceedings/einarsson/f90/r1mach.f90
    http://www.nsc.liu.se/~boein/ifip/kyoto/workshop-info/proceedings/einarsson/f90/i1mach.f90

All dependencies can be downloaded by

::

  wget http://people.sc.fsu.edu/~jburkardt/f_src/slatec/slatec.f90
  wget http://people.sc.fsu.edu/~jburkardt/f_src/f90split/f90split.f90
  wget http://www.nsc.liu.se/~boein/ifip/kyoto/workshop-info/proceedings/einarsson/f90/d1mach.f90
  wget http://www.nsc.liu.se/~boein/ifip/kyoto/workshop-info/proceedings/einarsson/f90/r1mach.f90
  wget http://www.nsc.liu.se/~boein/ifip/kyoto/workshop-info/proceedings/einarsson/f90/i1mach.f90

Other things needed by the script:

- ``gfortran`` (may be changed by editing the source)
- ``wget``
- ``bash``
  
How it works
============

The script ``install`` downloads the dependencies if necessary, and then
copies all files needed to a temporary directory. There, slatec.f90 will be
spilt into many f90 files, each containing one single function. Then, files
that would compile into object files with unresolvable dependencies (missing
user supplied functions) will be removed. All files will be compiled and
finally linked together. The shared object is then installed to the path
specified by the user.

To run the script, type

::

  ./install -p installation path

Notes
=====

The ``install`` scripts assumes you use a ``gfortran`` version that supports
``-march=native``. Changes can be made in the source file. There, also a
different compiler can be chosen. Also, the compilation of the individual
routines which is so far done by a ``bash`` script can be changed to some
other shell script.
