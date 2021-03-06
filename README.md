# Modular Aquatic Simulation System in One Dimension (MASS1)

Copyright (c) 2017 Battelle Memorial Institute
Licensed under modified BSD License. A copy of this license can be
found in the [LICENSE](LICENSE) file in the top level directory of this
distribution.

Home Page: http://mass1.pnnl.gov
Github repository: https://github.com/pnnl/mass1

## Description

MASS1 is a one-dimensional unsteady hydrodynamic and water quality
model capable of simulating open channel flows, water surface
elevations, dissolved gas, and water temperature. The model is
applicable to any branched channel system. Because MASS1 uses
cross-section averaging, only single values of water surface
elevation, velocity, discharge, and temperature are produced at each
cross-section location along the river course. Lateral (i.e., across
the river channel) and vertical variations of these quantities are not
simulated.  

MASS1 was developed by Pacific Northwest National Laboratory
(PNNL). It is a research code, written for applications and computer
platforms specific to the developers.  Others may, or may not,
find it useful.  

## Obtaining the Code

The MASS1 source code is currently hosted on PNNL's internal Git
repository.  MASS1 shares some code with other applications, which is
included in MASS1 as a Git sub-module.  Do the following to check out
the code:

```
git clone https://stash.pnnl.gov/scm/~d3g096/mass1.git
cd mass1
git submodule update --init
```

## Requirements

MASS1 has been used primarily on UNIX, Linux, and Mac OS X
systems. The following describes how to build MASS1 on those
platforms.  It has been built and used on Windows systems, but rarely.
The following should work on Windows if [Cygwin](https://cygwin.com/)
or [MinGW](http://www.mingw.org/) is utilized.  A native Windows build
may be available in the future. 

### Fortran 90/95 compiler

MASS1 is a relatively portable, vanilla Fortran 90/95 code.  It has
been built and used on UNIX, Linux, Mac OS X, and Windows.  A Fortran
90/95 compiler is required.  MASS1 is currently developed and tested
on Mac OS X and Linux using the following compilers:

* [GNU Fortran](https://gcc.gnu.org/fortran/), Version 4.1 or higher

* [Intel Fortran](https://software.intel.com/en-us/fortran-compilers),
  Version 10 or higher

In general, the Intel compiler produces a faster executable, but for
most applications, this is not noticable. In the past, Fortran
compilers from [NAG](https://www.nag.com/nag-compiler),
[Absoft](https://www.absoft.com/),
[IBM](http://www-03.ibm.com/software/products/en/xlfortran-linux), and
others have worked.  

### Perl

Several optional [Perl](https://www.perl.org/) pre/post-processing
scripts are provided.  These use the following
[Perl](https://www.perl.org/) modules:

* `Date::Manip`

* `PDL`

* `PDL::Graphics::PGPLOT::Window`

When MASS1 code is configured, the system's
[Perl](https://www.perl.org/) installation is queried to see if the
scripts can be used. If not, the scripts are not configured or
installed.  

### Python

In addition to the Perl scripts, some optional
[Python](https://www.python.org/) pre/post-processing scripts are also
provided.  

### R

There is an optional [R](https://www.r-project.org/) script for
reading MASS1 time series output and boundary condition files. 

## Configure and Build

MASS1 uses the [GNU Build System](https://en.wikipedia.org/wiki/GNU_Build_System) 
to build MASS1.  The `configure` script is used to query the system
and determine if the compiler has sufficient capability to compile
MASS1.  On most systems, simply executing the `configure` script is
sufficient, but sometimes options are required. To get a list of
options, 

```
./configure --help
```

A typical way to require the GNU Fortran compiler and trapping of
floating point errors:

```
./configure FC=gfortran FCFLAGS="-O2 -ffpe-trap=invalid,zero,overflow"
```

Similarly, for the Intel Fortran compiler:

```
./configure FC=ifort FCFLAGS="-O3 -fpe0"
```

## Usage

MASS1 is a *command line* application. There is no graphical user
interface.  All [input and output](doc/README.md) are plain text files.
There is very little [documentation](doc/README.md), other than the
code itself.  

The first step in a MASS1 application is to define the topology of the
river system to be simulated. The topological definition defines how
the channel system is connected as well as the location and type of
hydraulic control structures. The domain into a set of *links*, each
representing a single river reach. The ends of each link either
represent a boundary, at which river conditions are specified, or
connect to one or more other links. Each link is further divided into
series of computational points, where the hydrodynamic and transport
equations are discretized.  


When run, MASS1 expects to find a
[configuration file](doc/configuration.md) named `mass1.cfg`.  

## Citation

If you use MASS1, please cite 

   Richmond, M.C., Perkins, W.A., 2009. Efficient calculation of
   dewatered and entrapped areas using hydrodynamic modeling and
   GIS. *Environmental Modelling & Software* 24,
   1447–1456. doi:10.1016/j.envsoft.2009.06.001 

For examples of MASS1 use, see the [bibliography](doc/bibliography.md). 
