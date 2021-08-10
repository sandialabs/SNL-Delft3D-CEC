#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.68)
AC_INIT(Delft3D, 0.0, delft3d.support@deltares.nl)

dnl Store generated files in config dir
AC_CONFIG_AUX_DIR(config)
dnl Define a unique file to check if we're in the right directory
AC_CONFIG_SRCDIR([engines_gpl/flow2d3d/packages/kernel/src/compute/adi.f90])

AC_CONFIG_HEADER([config.h])
dnl m4 macro's in m4 directory.
AC_CONFIG_MACRO_DIR([m4])

dnl Look up target and host info (java needs this, should be called before init automake)
AC_CANONICAL_SYSTEM

dnl Initialise automake
AM_INIT_AUTOMAKE([subdir-objects])

# c, c++ and c preprocessor
# Problems may occur when mixing a C-compiler from one vendor (Gnu) with a F-compiler from another (Intel).
# The advice is to use the same vendor for both compilers (and the linker).
AC_PROG_CXX([icpc g++])
AC_PROG_CC([icc gcc])
AC_PROG_CC_C99
AC_PROG_CPP

dnl Initialize LIBTOOL
dnl Make sure we have an up to date version.
dnl We're going to make it work with older versions also, this might not work (libtool < 2.2, not sure how to check)

## ----------------------- ##
## Libtool initialisation. ##
## ----------------------- ##

LT_PREREQ(2.4)
LT_INIT
dnl LTDL_INIT dnl I don't think this is required anymore
dnl AC_PROG_LIBTOOL
AC_SUBST([LIBTOOL_DEPS])
AC_CONFIG_SUBDIRS(libltdl)
AC_SUBST(LIBLTDL)
AC_SUBST(INCLTDL)

#These are required for java
#AC_PROG_JAVA
#AC_PROG_JAVA_CC
#AC_PROG_JAVAC
#this is needed when sun java is unavailable
#AM_PROG_GCJ

# Other programs
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_SED
# These standard macros don't work well (don't throw an error on missing yacc)
# According to docs we can better use AM_PROG_LEX and AC_PROG_YACC
AC_PROG_YACC
AM_PROG_LEX
# So we'll double check
AC_CHECK_PROGS(LEXPROG, flex lex, :)
if test "x$LEXPROG" = "x:"; then
   AC_MSG_ERROR([Can't find lex, please install or set the LEX variable])
fi
AC_CHECK_PROGS(YACCPROG, 'bison -y' byacc yacc, :)
if test "x$YACCPROG" = "x:"; then
   AC_MSG_ERROR([Can't find yacc, please install or set the YACC variable])
fi

AC_LANG_PUSH([C++])

AC_CHECK_FUNCS([strdup vsnprintf lseek64 lseek])
AC_CHECK_FUNCS([fseeko64 ftello64])
AC_MSG_CHECKING([whether to enable dolcli])
AC_ARG_ENABLE(dolcli,
AS_HELP_STRING([--enable-dolcli],
[Enable the compilation of test executable delftonline/dolcli @<:@default no@:>@]),[enableval='yes'],[enableval='no'])
dnl set compiler specific options here:
if test "$enableval" = yes ; then
        AC_MSG_RESULT([yes])
        AC_CHECK_HEADERS([readline/history.h readline/readline.h],[],[AC_MSG_ERROR([Can't find readline header files. Please install libreadline6-dev (Ubuntu) or readline-devel (RedHat)])])
        dnl define a variable in the header file
        AC_DEFINE([ENABLE_DOLCLI], 1, [Use command line interface precision])
        dnl define the lowercase variable dolcli (see the substitute)
        dolcli=true
else
        AC_MSG_RESULT([no])
        dolcli=false
fi
dnl this variable ends up in the makefile.
AC_SUBST(dolcli)
AM_CONDITIONAL([ENABLE_DOLCLI], [test x$enableval = xyes])

AC_ARG_WITH(netcdf,[AS_HELP_STRING([--with-netcdf],[Build with netcdf support @<:@default=check@:>@])], [],[with_netcdf=check])
AS_IF([test "x$with_netcdf" != "xno"],
        [
        PKG_CHECK_MODULES([NETCDF], [netcdf])
        AC_DEFINE([HAVE_NETCDF],1,[Defined if you have NETCDF library.])
        ],
        [])
AM_CONDITIONAL([USENETCDF], test "x$with_netcdf" != "xno")
AC_LANG_POP

## This is copied from the libtool examples
## -------------------------- ##
## Fortran90 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran])
dnl http://www.astro.gla.ac.uk/users/norman/star/ssn78/ssn78.htx/N-x1a1b12.html
AC_PROG_FC([ifort gfortran])
dnl Check the flags needed to link fc programs with ld (i.e. cc)
AC_FC_LIBRARY_LDFLAGS
dnl Check for openmp for fortran
## AC_OPENMP

dnl Check for underscoring of external names
AC_FC_WRAPPERS
dnl We need to use .f90 and not .f to enable Automake FC support
dnl Some Intel ifc/ifort do not understand .f95.  :-/

AC_FC_SRCEXT([f90])
dnl Yes, we want free form Fortran!
AC_FC_FREEFORM
dnl Set unlimited line length (don't create lines longer than 132, because it breaks on some compilers,
dnl but because ifort and gfortran allow it let's not raise errors by default)
AC_FC_LINE_LENGTH([unlimited])
dnl check to see if we can write binary files
ACX_FC_FORM_BINARY

dnl check if open(...shared) works
ACX_FC_SHARED_SPECIFIER
dnl add preprocess option
AC_FC_PREPROCESS
AC_LANG_POP

## -------------------------- ##
## Fortran77 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran 77])
AC_PROG_F77([ifort gfortran g77])
AC_F77_LIBRARY_LDFLAGS
AC_F77_LINE_LENGTH([132])
dnl Check for openmp for fortran 77
## AC_OPENMP
dnl check if open(...shared) works
ACX_F77_SHARED_SPECIFIER

POLYPACK_FFLAGS=
AX_CHECK_COMPILE_FLAG([-r8],
        [
        # Intel ifort
        POLYPACK_FFLAGS=-r8
        ],[
        # GNU gfortran
        POLYPACK_FFLAGS=-fdefault-real-8
        ])
AC_SUBST(POLYPACK_FFLAGS)

AC_LANG_POP

# NetCDF, for fortran 90...
AC_LANG_PUSH([Fortran])
AC_ARG_WITH(netcdf,[AS_HELP_STRING([--with-netcdf],[Build with netcdf support @<:@default=check@:>@])], [],[with_netcdf=check])
AS_IF([test "x$with_netcdf" != "xno"],
        [
        # For netcdf >= 4.2 (separate fortran)
        # Check for netcdf 4 if not found...
        PKG_CHECK_MODULES([NETCDF_FORTRAN], [netcdf-fortran],[],[PKG_CHECK_MODULES(NETCDF, netcdf >= 4)])
        AC_DEFINE([HAVE_NETCDF],1,[Defined if you have NETCDF library.])
        ],
        [])
AM_CONDITIONAL([USENETCDF], test "x$with_netcdf" != "xno")
AC_LANG_POP

dnl Fortran compiler defaults (this could be a separate m4 file)
AC_MSG_CHECKING([whether using Fortran preset flags])
AC_ARG_ENABLE(fortran-flags-preset,
AS_HELP_STRING([--enable-fortran-flags-preset],
[Enable Fortran compiler flags preset @<:@default yes@:>@]),[],[enableval='yes'])
dnl set compiler specific options here:
if test "$enableval" = yes ; then
        AC_MSG_RESULT([yes])
        case "$F77" in
                *gfortran*) dnl GNU Fortran 95 compiler
                dnl no custom flags
                ;;
                *ifort*) dnl Intel 8.x Fortran compiler
                        FFLAGS="$FFLAGS -recursive -traceback"
                ;;
        esac
        case "$FC" in
             *gfortran*) dnl GNU Fortran 95 compiler
             dnl no custom flags
             ;;
             *ifort*) dnl Intel 8.x Fortran compiler
                      FCFLAGS="$FCFLAGS -recursive -traceback"
             ;;
        esac
else
        AC_MSG_RESULT([no])
fi




# As of the writing of this demo, GNU Autoconf's AC_OBJEXT and
# AC_EXEEXT macros only works for C compilers!
# Libtool's setup macro calls AC_OBJEXT and AC_EXEEXT without setting
# the test language to C.  We do it before any libtool setup macros are
# called so that the proper values are cached beforehand.  We also do
# it before any linker flags (LDFLAGS) are set so that fortran specific
# ones don't break the tests.
AC_LANG_PUSH([C])
AC_OBJEXT
AC_LANG_POP


# executables will get an .exe extension
AC_EXEEXT([.exe])




dnl define options
dnl Use double support
AC_MSG_CHECKING([whether to disable double support])
AC_ARG_ENABLE(double,
[  --disable-double           Disable double support],
   [ AC_MSG_RESULT([yes]) ],
   [ AC_MSG_RESULT([no]); AC_DEFINE(WITH_DOUBLE, 1, [Use double precision]) ]
)


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([libgen.h arpa/inet.h fcntl.h netdb.h netinet/in.h stdlib.h string.h sys/socket.h sys/time.h errno.h unistd.h malloc.h malloc/malloc.h sys/ipc.h sys/shm.h])

#check for jni.h (for later)
#AC_JNI_INCLUDE_DIR

#check for c++ standard library
AC_CHECK_LIB([stdc++], [main],, [AC_MSG_ERROR([Compiler can't find libstdc++])])
AC_CHECK_TYPES([struct shm_info])
#Check for expat
AC_CHECK_HEADER(expat.h,, [AC_MSG_ERROR([Compiler can't find <expat.h>])])
AC_CHECK_LIB(expat, XML_Parse,, [AC_MSG_ERROR([Compiler can't find libexpat])])
AC_CHECK_LIB([rt], [clock_gettime],, [AC_MSG_WARN([Compiler can't find librt, needed for clock_gettime])])
AC_FUNC_VPRINTF
# When using the Intel Fortran compiler, check for the needed libraries
####if test $FC == ifort; then
####    AC_CHECK_LIB([irc], [main], [HAVE_LIBirc=1] [FCLIBS="$FCLIBS -lirc"],)
####    AC_CHECK_LIB([svml], [main], [HAVE_LIBsvml=1] [FCLIBS="$FCLIBS -lsvml"],)
####    AC_CHECK_LIB([mcheck], [main], [HAVE_LIBmcheck=1] [FCLIBS="$FCLIBS -lmcheck"],)
####fi

# checks for pthreads
####ACX_PTHREAD([enable_threads="pthread"],[enable_threads="no"])
####
####if test $enable_threads != "pthread"; then
####    AC_MSG_ERROR([unable to find pthreads, currently this is required])
####else
####    AC_DEFINE(HAVE_PTHREAD,1,[Define if you have POSIX threads libraries and header files.])
####    LIBS="$PTHREAD_LIBS $LIBS"
####    FCLIBS="$PTHREAD_LIBS $FCLIBS"
####    CFLAGS="$PTHREAD_CFLAGS $CFLAGS"
####    CXXFLAGS="$PTHREAD_CFLAGS $CXXFLAGS"
####fi


# Make sure we throw errors when using invalid architecture files (otherwise OSX version might get linked against linux libs)


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
dnl AC_FUNC_MALLOC must probably be removed on OSX
AC_FUNC_MALLOC
AC_HEADER_MAJOR
dnl AC_FUNC_REALLOC must probably be removed on OSX
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([gethostname gettimeofday memset strerror])

# Checks for QT library functions.
# gw_CHECK_QT

dnl Check for MPI (sets MPIFC, MPILIBS, HAVE_MPI), compile everything with mpi if mpi is used (c++ + fortran)
AC_ARG_WITH(mpi, [AS_HELP_STRING([--with-mpi],[Build with mpi support @<:@default=yes@:>@])], [],[with_mpi=yes])
AS_IF([test "x$with_mpi" != xno],
[
AC_LANG_PUSH([C++])
ACX_MPI
AC_LANG_POP

dnl needed for linking
AC_LANG_PUSH([Fortran 77])
ACX_MPI
AC_LANG_POP

AC_LANG_PUSH([Fortran])
ACX_MPI
AC_LANG_POP

F77=$MPIF77
FC=$MPIFC
CXX=$MPICXX
dnl add the fortran f90 libraries to the link flags, in case mpicxx is used as linker
MPILIBS="$MPILIBS $($MPIFC -showme:link)"
    MPILIBS="$MPILIBS $($MPIFC -show 2>/dev/null | cut -d" " -f 2- )"
],
[])
AM_CONDITIONAL(HAVE_MPI, [test "x$with_mpi" != xno])

# define usempi (same as HAVE_MPI)
# Conditionals like this should not be needed but I don't know a shortcut
AM_CONDITIONAL([USEMPI], test "x$with_mpi" != "xno")

#
# Checks for OpenDA if user selects this.
#
# User gives path to OpenDA libraries using --with-openda
INT_OPENDA="todo";
INT_OPENDA_VERSION="NO"
ODA_SRC=""
ODA_INCDIR=""
ODA_LIB=""
LTL_CTA=""
AC_ARG_WITH(openda,
              AS_HELP_STRING([--with-openda=PATH],
                             [Try to use 'PATH/include' to find the OPENDA
                              headers and 'PATH/lib' to find the OPENDA libraries.]),
      [
       OPENDA_DIR=${withval}
       with_oda=yes
       ODA_SRC=flow2d3d_openda
       ODA_INCDIR="-I$OPENDA_DIR/include"
       LTL_CTA="$OPENDA_DIR/lib/libcta.la"
       ODA_LIB=" -lcta"
      ],[])
AC_SUBST(ODA_SRC)
AC_SUBST(ODA_INCDIR)
AC_SUBST(ODA_LIB)
AC_SUBST(LTL_CTA)
AM_CONDITIONAL([WITH_ODA],[test "$with_oda" = yes])


dnl Already create the libtool script here, because we're using it to detect .dylib/.so extension (works for >=2.2)
LT_OUTPUT

dnl Lookup the library extension (usually: so,sl,dylib,dll)
AC_MSG_CHECKING(for the extension of Delft3D plugins)
shrext_cmds=$(./libtool --config | grep '^shrext_cmds=')
eval $shrext_cmds
module=no eval pluginext="$shrext_cmds"
AC_MSG_RESULT($pluginext)
if test -z "$pluginext"; then
  AC_MSG_ERROR([can't detect plugin extension])
fi
AC_DEFINE_UNQUOTED(D3D_PLUGIN_EXT, ["$pluginext"], [Delft3D plugin extension])

################ DflowFM extras #############################

# Check for Intel MKL
ACX_MKL
AM_CONDITIONAL([USEMKL], test "x$ax_mkl_ok" == "xyes")

# Check for PETSc
AC_MSG_CHECKING([if PETSc is wanted])
AC_ARG_WITH(	    petsc,
		    [AS_HELP_STRING([--with-petsc],[Build with petsc support  @<:@default=no@:>@])],
		    [],
		    [with_petsc=no])

AS_IF([test "x$with_petsc" != "xno"],
	    	[
			AC_MSG_RESULT([yes])			
			# now supporting 3.8 onwards, and deprecated 3.4
			PKG_CHECK_MODULES([PETSc], [PETSc > 3.4 PETSc < 3.5],
			[
				AC_MSG_WARN([PETSc 3.4 has been deprecated, please consider upgrading to version 3.8 or higher])
				AC_DEFINE([USE_DEPRECATED_PETSC34], 1, [Defined for building with deprecated PETSc version 3.4])
			],
			[
				PKG_CHECK_MODULES([PETSc], [PETSc >= 3.8])					
			])
			# do we need fortran cflags here??
			AC_DEFINE([HAVE_PETSC],1,[Defined if you have PETSc library.])
			
		],
	    	AC_MSG_RESULT([no]))

AM_CONDITIONAL([USEPETSC], test "x$with_petsc" != "xno")

dnl Check for METIS
AC_MSG_CHECKING([if Metis is wanted])
AC_ARG_WITH(metis,[AS_HELP_STRING([--with-metis[=DIR]],[Build with metis support  @<:@default=no@:>@])],[],[with_metis=no])

METIS_DIR=""
AS_IF([test "x$with_metis" != "xno"],
	[
		AC_DEFINE([HAVE_METIS],1,[Defined if you have Metis library.])
		AS_IF([test "x$with_metis" != "xyes"],
		[
		  METIS_DIR=$with_metis
		])
		AC_MSG_RESULT([yes])
	],
	[
		AC_MSG_RESULT([no])
	])
AM_CONDITIONAL([USEMETIS], test "x$with_metis" != "xno")

dnl Check for BLAS
AC_MSG_CHECKING([if BLAS is wanted])
AC_ARG_WITH(
    blas,
    AS_HELP_STRING(
       [--with-blas=DIR],
       [Root directory with BLAS includes and libraries @<:@/usr@:>@]
    ),
    [blasdir="$withval"],
    [blasdir=]
)
if test "xno" != "x$blasdir"; then
    if test -d "${blasdir}"; then
        AC_MSG_RESULT([yes])
        AC_DEFINE([HAVE_BLAS], 1,[Defined if you have BLAS  library.])
        AC_DEFINE([HAVE_PARMS],1,[Defined if you have PARMS library.])
    else
        AC_MSG_RESULT([no])
    fi
else
  AC_MSG_RESULT([no])
fi
AC_SUBST(blasdir)
AM_CONDITIONAL([USEPARMS], test "x$withblas" != "xno" -a -d "${blasdir}")

dnl Check for PROJ
dnl Note that the option below is a --with-PKG=dir option,
dnl whereas the third_party_open/fortrangis/ uses a --enable-PKG option.
dnl As a result the include/lib paths are not automatically correct,
dnl so we set them below in the central CPPFLAGS and LDFLAGS.
AC_MSG_CHECKING([if PROJ is wanted])
AC_ARG_WITH(proj,[AS_HELP_STRING([--with-proj[=DIR]],[Build with PROJ support  @<:@default=no@:>@])],[],[with_proj=no])

PROJ_DIR=""
AS_IF([test "x$with_proj" != "xno"],
	[
		AC_DEFINE([HAVE_PROJ],1,[Defined if you have PROJ library.])
		AS_IF([test "x$with_proj" != "xyes"],
		[
		  PROJ_DIR=$with_proj
		])
		AC_MSG_RESULT([yes])
	],
	[
		AC_MSG_RESULT([no])
	])
AM_CONDITIONAL([USEPROJ], test "x$with_proj" != "xno")

dnl Check for SHAPELIB
dnl Note that the option below is a --with-PKG=dir option,
dnl whereas the third_party_open/fortrangis/ uses a --enable-PKG option.
dnl As a result the include/lib paths are not automatically correct,
dnl so we set them below in the central CPPFLAGS and LDFLAGS.
AC_MSG_CHECKING([if Shapelib is wanted])
AC_ARG_WITH(shapelib,[AS_HELP_STRING([--with-shapelib[=DIR]],[Build with shapelib support  @<:@default=no@:>@])],[],[with_shapelib=no])

SHAPELIB_DIR=""
AS_IF([test "x$with_shapelib" != "xno"],
	[
		AC_DEFINE([HAVE_SHAPELIB],1,[Defined if you have Shapelib library.])
		AS_IF([test "x$with_shapelib" != "xyes"],
		[
		  SHAPELIB_DIR=$with_shapelib
		])
		AC_MSG_RESULT([yes])
	],
	[
			AC_MSG_RESULT([no])
	])
AM_CONDITIONAL([USESHAPELIB], test "x$with_shapelib" != "xno")
if test "x" != "x$SHAPELIB_DIR"; then
    if test -d "${SHAPELIB_DIR}"; then
        AX_APPEND_FLAG("-I${SHAPELIB_DIR}/include", CPPFLAGS)
        AX_APPEND_FLAG("-L${SHAPELIB_DIR}/lib", LDFLAGS)
    fi
fi

AM_CONDITIONAL([USEFORTRANGIS], test "x$with_shapelib" != "xno" -o "x$with_proj" != "xno")

AC_LANG_PUSH([C])
AC_CHECK_LIB([dl], [dlclose],, [AC_MSG_ERROR([Compiler can not find libdl])])

# Check for Metis headers and set-up of flags
AS_IF([test "x$with_metis" != "xno"], [
	dnl # Setup temp LDFLAGS and look for METIS library/header
	LDFLAGS_SAVE=${LDFLAGS};
  	CPPFLAGS_SAVE=${CPPFLAGS};
  	AC_ARG_VAR([METIS_CFLAGS], [C compiler flags for using METIS headers])
  	AC_ARG_VAR([METIS_LIBS], [linker flags for using METIS libraries])

	# Check for Metis headers
  	AS_IF([test "x$METIS_CFLAGS" != "x"], [
  		AX_APPEND_FLAG($METIS_CFLAGS, CPPFLAGS) ],
  	[
  		AS_IF([test "x$METIS_DIR" != "x"],
  		[
      	dnl # Look for METIS on the user specified inky install path
      		METIS_CFLAGS="-I$METIS_DIR/include";
      		AX_APPEND_FLAG($METIS_CFLAGS, CPPFLAGS)
  		])
    ])
  	AS_IF([test "x$METIS_LIBS" != "x"], [
  		AX_APPEND_FLAG($METIS_LIBS, LDFLAGS) ],
  	[
  		AS_IF([test "x$METIS_DIR" != "x"],
  		[
      	dnl # Look for METIS on the user specified inky install path
      		METIS_LIBS="-L$METIS_DIR/lib";
      		AX_APPEND_FLAG($METIS_LIBS, LDFLAGS)
  		])
    	METIS_LIBS="$METIS_LIBS -lmetis"
    ])

  	AC_LANG_PUSH([C])
   	 AC_CHECK_HEADER(metis.h, [], [AC_MSG_ERROR([METIS headers not found. Install METIS.])])
     AC_CHECK_LIB(metis, [METIS_PARTMESHDUAL], [foo=bar],[AC_MSG_ERROR([METIS library not found. Install METIS.])])
  	AC_LANG_POP()

    dnl # Restore original LDFLAGS
    LDFLAGS=$LDFLAGS_SAVE;
    CPPFLAGS=$CPPFLAGS_SAVE;
])

AC_LANG_POP

###############



#-------------------------------------------------------------------------------
#   List of makefiles to generate.  This list can be created with the command:
#   find . -name Makefile.am | egrep '/.+/' | sort | sed 's/.am$//'

AC_CONFIG_FILES([Makefile
engines_gpl/dflowfm/Makefile
engines_gpl/dflowfm/packages/dflowfm-cli_exe/Makefile
engines_gpl/dflowfm/packages/dflowfm-cli_exe/src/Makefile
engines_gpl/dflowfm/packages/dflowfm_kernel/Makefile
engines_gpl/dflowfm/packages/dflowfm_kernel/src/Makefile
engines_gpl/dflowfm/packages/Makefile
engines_gpl/dflowfm/scripts/parallel/Makefile
engines_gpl/dflowfm/scripts/team-city/Makefile
engines_gpl/dflowfm/scripts/Makefile
engines_gpl/d_hydro/Makefile
engines_gpl/d_hydro/packages/d_hydro/Makefile
engines_gpl/d_hydro/packages/d_hydro/src/Makefile
engines_gpl/d_hydro/packages/Makefile
engines_gpl/dimr/Makefile
engines_gpl/dimr/packages/dimr_lib/include/Makefile
engines_gpl/dimr/packages/dimr_lib/Makefile
engines_gpl/dimr/packages/dimr_lib/src/Makefile
engines_gpl/dimr/packages/dimr/Makefile
engines_gpl/dimr/packages/dimr/src/Makefile
engines_gpl/dimr/packages/Makefile
engines_gpl/dimr/scripts/generic/lnx64/Makefile
engines_gpl/dimr/scripts/generic/Makefile
engines_gpl/dimr/scripts/Makefile
engines_gpl/flow2d3d/default/Makefile
engines_gpl/flow2d3d/Makefile
engines_gpl/flow2d3d/packages/data/Makefile
engines_gpl/flow2d3d/packages/data/src/allocation/Makefile
engines_gpl/flow2d3d/packages/data/src/basics/Makefile
engines_gpl/flow2d3d/packages/data/src/gdp/Makefile
engines_gpl/flow2d3d/packages/data/src/general/Makefile
engines_gpl/flow2d3d/packages/data/src/Makefile
engines_gpl/flow2d3d/packages/data/src/parallel_mpi/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/Makefile
engines_gpl/flow2d3d/packages/flow2d3d_openda/Makefile
engines_gpl/flow2d3d/packages/flow2d3d_openda/src/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/dd/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/Makefile
engines_gpl/flow2d3d/packages/io_dol_f/Makefile
engines_gpl/flow2d3d/packages/io_dol_f/src/Makefile
engines_gpl/flow2d3d/packages/io/Makefile
engines_gpl/flow2d3d/packages/io/src/input/Makefile
engines_gpl/flow2d3d/packages/io/src/Makefile
engines_gpl/flow2d3d/packages/io/src/output/Makefile
engines_gpl/flow2d3d/packages/io/src/preprocessor/Makefile
engines_gpl/flow2d3d/packages/kernel_dd_f/Makefile
engines_gpl/flow2d3d/packages/kernel_dd_f/src/Makefile
engines_gpl/flow2d3d/packages/kernel/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute_roller/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/Makefile
engines_gpl/flow2d3d/packages/kernel/src/dd/Makefile
engines_gpl/flow2d3d/packages/kernel/src/general/Makefile
engines_gpl/flow2d3d/packages/kernel/src/inichk/Makefile
engines_gpl/flow2d3d/packages/kernel/src/main/Makefile
engines_gpl/flow2d3d/packages/kernel/src/Makefile
engines_gpl/flow2d3d/packages/kernel/src/non_hydro/Makefile
engines_gpl/flow2d3d/packages/kernel/src/taylor/Makefile
engines_gpl/flow2d3d/packages/kernel/src/timedep/Makefile
engines_gpl/flow2d3d/packages/Makefile
engines_gpl/flow2d3d/packages/manager/Makefile
engines_gpl/flow2d3d/packages/manager/src/Makefile
engines_gpl/flow2d3d/packages/plugin_culvert_c/Makefile
engines_gpl/flow2d3d/packages/plugin_culvert_c/src/Makefile
engines_gpl/flow2d3d/packages/plugin_user/Makefile
engines_gpl/flow2d3d/packages/plugin_user/src/Makefile
engines_gpl/flow2d3d/scripts/Makefile
engines_gpl/Makefile
engines_gpl/part/Makefile
engines_gpl/part/packages/data_f/Makefile
engines_gpl/part/packages/data_f/src/Makefile
engines_gpl/part/packages/delpar/Makefile
engines_gpl/part/packages/delpar/src/Makefile
engines_gpl/part/packages/io_f/Makefile
engines_gpl/part/packages/io_f/src/Makefile
engines_gpl/part/packages/kernel_f/Makefile
engines_gpl/part/packages/kernel_f/src/Makefile
engines_gpl/part/packages/Makefile
engines_gpl/part/packages/utils_f/Makefile
engines_gpl/part/packages/utils_f/src/Makefile
engines_gpl/part/scripts/Makefile
engines_gpl/waq/default/Makefile
engines_gpl/waq/Makefile
engines_gpl/waq/packages/delwaq1/Makefile
engines_gpl/waq/packages/delwaq1/src/Makefile
engines_gpl/waq/packages/delwaq2/Makefile
engines_gpl/waq/packages/delwaq2/src/Makefile
engines_gpl/waq/packages/delwaq_lib_examples/delwaq2_stepwise/src/Makefile
engines_gpl/waq/packages/delwaq_lib_examples/delwaq_dimr_test/src/Makefile
engines_gpl/waq/packages/delwaq_lib_examples/unit_tests_waq/Makefile
engines_gpl/waq/packages/delwaq_lib_examples/unit_tests_waq/src/Makefile
engines_gpl/waq/packages/delwaq/Makefile
engines_gpl/waq/packages/delwaq/src/Makefile
engines_gpl/waq/packages/Makefile
engines_gpl/waq/packages/waq_io/Makefile
engines_gpl/waq/packages/waq_io/src/Makefile
engines_gpl/waq/packages/waq_io/src/mpidum/Makefile
engines_gpl/waq/packages/waq_io/src/ods/Makefile
engines_gpl/waq/packages/waq_io/src/waq_io/Makefile
engines_gpl/waq/packages/waq_kernel/Makefile
engines_gpl/waq/packages/waq_kernel/src/Makefile
engines_gpl/waq/packages/waq_plugin_wasteload/Makefile
engines_gpl/waq/packages/waq_plugin_wasteload/src/Makefile
engines_gpl/waq/packages/waq_process/Makefile
engines_gpl/waq/packages/waq_process/src/bloom/Makefile
engines_gpl/waq/packages/waq_process/src/charon/Makefile
engines_gpl/waq/packages/waq_process/src/Makefile
engines_gpl/waq/packages/waq_process/src/proc_preprocess/Makefile
engines_gpl/waq/packages/waq_process/src/waq_process/Makefile
engines_gpl/waq/packages/waq_utils_c/Makefile
engines_gpl/waq/packages/waq_utils_c/src/Makefile
engines_gpl/waq/packages/waq_utils_f/Makefile
engines_gpl/waq/packages/waq_utils_f/src/dlwq_netcdf/Makefile
engines_gpl/waq/packages/waq_utils_f/src/Makefile
engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/Makefile
engines_gpl/waq/packages/waq_utils_f/src/mod_timings/Makefile
engines_gpl/waq/packages/waq_utils_f/src/waq_utils/Makefile
engines_gpl/waq/packages/wq_processes/Makefile
engines_gpl/waq/packages/wq_processes/src/Makefile
engines_gpl/waq/scripts/Makefile
engines_gpl/wave/Makefile
engines_gpl/wave/packages/data/Makefile
engines_gpl/wave/packages/data/src/Makefile
engines_gpl/wave/packages/io/Makefile
engines_gpl/wave/packages/io/src/Makefile
engines_gpl/wave/packages/kernel/Makefile
engines_gpl/wave/packages/kernel/src/Makefile
engines_gpl/wave/packages/Makefile
engines_gpl/wave/packages/manager/Makefile
engines_gpl/wave/packages/manager/src/Makefile
engines_gpl/wave/packages/wave/Makefile
engines_gpl/wave/packages/wave/src/Makefile
engines_gpl/wave/scripts/Makefile
plugins_lgpl/Makefile
plugins_lgpl/plugin_culvert/Makefile
plugins_lgpl/plugin_culvert/src/Makefile
plugins_lgpl/plugin_delftflow_traform/Makefile
plugins_lgpl/plugin_delftflow_traform/src/Makefile
scripts_lgpl/linux/Makefile
scripts_lgpl/Makefile
test/Makefile
test/engines_gpl/Makefile
test/engines_gpl/waq/Makefile
test/engines_gpl/waq/packages/Makefile
test/engines_gpl/waq/packages/waq_utils_f/Makefile
test/engines_gpl/waq/packages/waq_utils_f/src/Makefile
test/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/Makefile
test/engines_gpl/dflowfm/Makefile
test/engines_gpl/dflowfm/packages/Makefile
test/engines_gpl/dflowfm/packages/dflowfm_kernel/Makefile
test/engines_gpl/dflowfm/packages/dflowfm_kernel/src/Makefile
test/utils_lgpl/deltares_common/Makefile
test/utils_lgpl/deltares_common/packages/Makefile
test/utils_lgpl/deltares_common/packages/test_deltares_common/Makefile
test/utils_lgpl/deltares_common/packages/test_deltares_common/src/Makefile
test/utils_lgpl/Makefile
third_party/interacter_stub/Makefile
third_party/interacter_stub/src/Makefile
third_party/Makefile
third_party_open/esmf/lnx64/bin/Makefile
third_party_open/esmf/lnx64/Makefile
third_party_open/esmf/lnx64/scripts/Makefile
third_party_open/esmf/Makefile
third_party_open/Makefile
third_party_open/md5/Makefile
third_party_open/md5/md5digest/Makefile
third_party_open/md5/md5file/Makefile
third_party_open/polypack/Makefile
third_party_open/polypack/src/Makefile
third_party_open/swan/bin/linux/Makefile
third_party_open/swan/bin/Makefile
third_party_open/swan/Makefile
third_party_open/swan/scripts/Makefile
third_party_open/triangle/Makefile
third_party_open/version_number/Makefile
third_party_open/version_number/packages/Makefile
third_party_open/version_number/packages/version_number/Makefile
third_party_open/version_number/packages/version_number/src/Makefile
tools_gpl/datsel/Makefile
tools_gpl/datsel/packages/datsel_f/Makefile
tools_gpl/datsel/packages/datsel_f/src/Makefile
tools_gpl/datsel/packages/datsel/Makefile
tools_gpl/datsel/packages/datsel/src/Makefile
tools_gpl/datsel/packages/datsel_version_number/Makefile
tools_gpl/datsel/packages/datsel_version_number/src/Makefile
tools_gpl/datsel/packages/Makefile
tools_gpl/dfmoutput/Makefile
tools_gpl/dfmoutput/src/Makefile
tools_gpl/kubint/Makefile
tools_gpl/kubint/packages/kubint_f/Makefile
tools_gpl/kubint/packages/kubint_f/src/Makefile
tools_gpl/kubint/packages/kubint/Makefile
tools_gpl/kubint/packages/kubint/src/Makefile
tools_gpl/kubint/packages/kubint_version_number/Makefile
tools_gpl/kubint/packages/kubint_version_number/src/Makefile
tools_gpl/kubint/packages/Makefile
tools_gpl/lint/Makefile
tools_gpl/lint/packages/lint_f/Makefile
tools_gpl/lint/packages/lint_f/src/Makefile
tools_gpl/lint/packages/lint/Makefile
tools_gpl/lint/packages/lint/src/Makefile
tools_gpl/lint/packages/lint_version_number/Makefile
tools_gpl/lint/packages/lint_version_number/src/Makefile
tools_gpl/lint/packages/Makefile
tools_gpl/Makefile
tools_gpl/mormerge/Makefile
tools_gpl/mormerge/packages/Makefile
tools_gpl/mormerge/packages/mormerge/Makefile
tools_gpl/mormerge/packages/mormerge/src/Makefile
tools_gpl/mormerge/packages/mormerge_version_number/Makefile
tools_gpl/mormerge/packages/mormerge_version_number/src/Makefile
tools_gpl/nesthd1/Makefile
tools_gpl/nesthd1/packages/Makefile
tools_gpl/nesthd1/packages/nesthd1/Makefile
tools_gpl/nesthd1/packages/nesthd1/src/Makefile
tools_gpl/nesthd2/Makefile
tools_gpl/nesthd2/packages/Makefile
tools_gpl/nesthd2/packages/nesthd2/Makefile
tools_gpl/nesthd2/packages/nesthd2/src/Makefile
tools_gpl/vs/Makefile
tools_gpl/vs/packages/Makefile
tools_gpl/vs/packages/vs/Makefile
tools_gpl/vs/packages/vs/src/Makefile
tools_gpl/vs/packages/vs_version_number/Makefile
tools_gpl/vs/packages/vs_version_number/src/Makefile
tools_gpl/waq_run_processes/Makefile
tools_gpl/waq_run_processes/packages/Makefile
tools_gpl/waq_run_processes/packages/waq_run_processes/Makefile
tools_gpl/waq_run_processes/packages/waq_run_processes/src/Makefile
tools_gpl/waq_run_processes/packages/waq_run_processes_version_number/Makefile
tools_gpl/waq_run_processes/packages/waq_run_processes_version_number/src/Makefile
tools_gpl/waqpb/Makefile
tools_gpl/waqpb/packages/Makefile
tools_gpl/waqpb/packages/waqpb_export/Makefile
tools_gpl/waqpb/packages/waqpb_export/src/Makefile
tools_gpl/waqpb/packages/waqpb_import/Makefile
tools_gpl/waqpb/packages/waqpb_import/src/Makefile
tools_gpl/waqpb/packages/waqpb_lib/Makefile
tools_gpl/waqpb/packages/waqpb_lib/src/Makefile
utils_gpl/flow1d/include/Makefile
utils_gpl/flow1d/Makefile
utils_gpl/flow1d/packages/flow1d_core/Makefile
utils_gpl/flow1d/packages/flow1d_core/src/Makefile
utils_gpl/flow1d/packages/flow1d_io/Makefile
utils_gpl/flow1d/packages/flow1d_io/src/Makefile
utils_gpl/flow1d/packages/flow1d/Makefile
utils_gpl/flow1d/packages/flow1d/src/Makefile
utils_gpl/flow1d/packages/Makefile
utils_gpl/Makefile
utils_gpl/morphology/include/Makefile
utils_gpl/morphology/Makefile
utils_gpl/morphology/packages/Makefile
utils_gpl/morphology/packages/morphology_data/Makefile
utils_gpl/morphology/packages/morphology_data/src/Makefile
utils_gpl/morphology/packages/morphology_io/Makefile
utils_gpl/morphology/packages/morphology_io/src/Makefile
utils_gpl/morphology/packages/morphology_kernel/Makefile
utils_gpl/morphology/packages/morphology_kernel/src/Makefile
utils_gpl/morphology/packages/morphology_plugins_c/Makefile
utils_gpl/morphology/packages/morphology_plugins_c/src/Makefile
utils_gpl/morphology/packages/morphology_proclib/Makefile
utils_gpl/morphology/packages/morphology_waq/Makefile
utils_gpl/morphology/packages/morphology_waq/src/Makefile
utils_gpl/trachytopes/Makefile
utils_gpl/trachytopes/packages/Makefile
utils_gpl/trachytopes/packages/trachytopes_io/Makefile
utils_gpl/trachytopes/packages/trachytopes_io/src/Makefile
utils_gpl/trachytopes/packages/trachytopes_kernel/Makefile
utils_gpl/trachytopes/packages/trachytopes_kernel/src/Makefile
utils_lgpl/delftio/include/Makefile
utils_lgpl/delftio/Makefile
utils_lgpl/delftio/packages/delftio/Makefile
utils_lgpl/delftio/packages/delftio_shm/include/Makefile
utils_lgpl/delftio/packages/delftio_shm/Makefile
utils_lgpl/delftio/packages/delftio_shm/src/diof90/Makefile
utils_lgpl/delftio/packages/delftio_shm/src/Makefile
utils_lgpl/delftio/packages/delftio/src/delftio_sync/Makefile
utils_lgpl/delftio/packages/delftio/src/diof90/Makefile
utils_lgpl/delftio/packages/delftio/src/diof90Nefis/Makefile
utils_lgpl/delftio/packages/delftio/src/Makefile
utils_lgpl/delftio/packages/Makefile
utils_lgpl/delftonline/include/Makefile
utils_lgpl/delftonline/Makefile
utils_lgpl/delftonline/src/delftonline/Makefile
utils_lgpl/delftonline/src/dolcli/Makefile
utils_lgpl/delftonline/src/dolprint/Makefile
utils_lgpl/deltares_common/Makefile
utils_lgpl/deltares_common/packages/deltares_common_c/include/Makefile
utils_lgpl/deltares_common/packages/deltares_common_c/Makefile
utils_lgpl/deltares_common/packages/deltares_common_c/src/Makefile
utils_lgpl/deltares_common/packages/deltares_common/Makefile
utils_lgpl/deltares_common/packages/deltares_common_mpi/Makefile
utils_lgpl/deltares_common/packages/deltares_common_mpi/src/Makefile
utils_lgpl/deltares_common/packages/deltares_common/src/Makefile
utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/Makefile
utils_lgpl/deltares_common/packages/Makefile
utils_lgpl/deltares_common/tests/02_streams_smalltest/Makefile
utils_lgpl/deltares_common/tests/03_streams_test/Makefile
utils_lgpl/deltares_common/tests/06_inifiles_test/Makefile
utils_lgpl/deltares_common/tests/07_inifiles_treetest/Makefile
utils_lgpl/deltares_common/tests/08_inifiles_dredgedumptest/Makefile
utils_lgpl/deltares_common/tests/09_system_info/Makefile
utils_lgpl/deltares_common/tests/Makefile
utils_lgpl/d_hydro_lib/include/Makefile
utils_lgpl/d_hydro_lib/Makefile
utils_lgpl/d_hydro_lib/packages/d_hydro_lib/include/Makefile
utils_lgpl/d_hydro_lib/packages/d_hydro_lib/Makefile
utils_lgpl/d_hydro_lib/packages/d_hydro_lib/src/Makefile
utils_lgpl/d_hydro_lib/packages/Makefile
utils_lgpl/ec_module/Makefile
utils_lgpl/ec_module/packages/ec_module/Makefile
utils_lgpl/ec_module/packages/ec_module/src/ectest/Makefile
utils_lgpl/ec_module/packages/ec_module/src/Makefile
utils_lgpl/ec_module/packages/ec_module/src/meteo/Makefile
utils_lgpl/ec_module/packages/ec_module_test/Makefile
utils_lgpl/ec_module/packages/ec_module_test/src/Makefile
utils_lgpl/ec_module/packages/Makefile
utils_lgpl/esmfsm/include/Makefile
utils_lgpl/esmfsm/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/include/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/src/Makefile
utils_lgpl/esmfsm/packages/esmfsm/Makefile
utils_lgpl/esmfsm/packages/esmfsm/src/fsm/Makefile
utils_lgpl/esmfsm/packages/esmfsm/src/Makefile
utils_lgpl/esmfsm/packages/esmfsm_version_number/include/Makefile
utils_lgpl/esmfsm/packages/esmfsm_version_number/Makefile
utils_lgpl/esmfsm/packages/esmfsm_version_number/src/Makefile
utils_lgpl/esmfsm/packages/Makefile
utils_lgpl/esmfsm/tests/allocate_local/Makefile
utils_lgpl/esmfsm/tests/esm_create/Makefile
utils_lgpl/esmfsm/tests/esm_delete/Makefile
utils_lgpl/esmfsm/tests/esm_info/Makefile
utils_lgpl/esmfsm/tests/Makefile
utils_lgpl/esmfsm/tests/test_01/Makefile
utils_lgpl/esmfsm/tests/test_02/Makefile
utils_lgpl/esmfsm/tests/test_03/Makefile
utils_lgpl/esmfsm/tests/testesm/Makefile
utils_lgpl/esmfsm/tests/testfill/Makefile
utils_lgpl/esmfsm/tests/testfsm/Makefile
utils_lgpl/ftnunit/Makefile
utils_lgpl/ftnunit/packages/ftnunit/Makefile
utils_lgpl/ftnunit/packages/ftnunit/src/Makefile
utils_lgpl/ftnunit/packages/Makefile
utils_lgpl/gridgeom/Makefile
utils_lgpl/gridgeom/packages/gridgeom/Makefile
utils_lgpl/gridgeom/packages/gridgeom/src/Makefile
utils_lgpl/gridgeom/packages/Makefile
utils_lgpl/io_hyd/Makefile
utils_lgpl/io_hyd/packages/io_hyd/Makefile
utils_lgpl/io_hyd/packages/io_hyd/src/Makefile
utils_lgpl/io_hyd/packages/Makefile
utils_lgpl/io_netcdf/Makefile
utils_lgpl/io_netcdf/packages/io_netcdf/Makefile
utils_lgpl/io_netcdf/packages/io_netcdf/src/Makefile
utils_lgpl/io_netcdf/packages/Makefile
utils_lgpl/kdtree_wrapper/Makefile
utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/Makefile
utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/Makefile
utils_lgpl/kdtree_wrapper/packages/Makefile
utils_lgpl/Makefile
utils_lgpl/metistools/Makefile
utils_lgpl/metistools/packages/Makefile
utils_lgpl/metistools/packages/metisoptions/Makefile
utils_lgpl/metistools/packages/metisoptions/src/Makefile
utils_lgpl/nefis/Makefile
utils_lgpl/nefis/packages/Makefile
utils_lgpl/nefis/packages/nefis/Makefile
utils_lgpl/nefis/packages/nefis/src/Makefile
utils_lgpl/nefis/packages/nefis/src_so/Makefile
utils_lgpl/nefis/tests/demo_00/Makefile
utils_lgpl/nefis/tests/demo_01/Makefile
utils_lgpl/nefis/tests/demo_02/Makefile
utils_lgpl/nefis/tests/Makefile
utils_lgpl/nefis/tests/observations/Makefile
utils_lgpl/nefis/tests/test_00/Makefile
utils_lgpl/nefis/tests/test_01/Makefile
utils_lgpl/nefis/tests/test_02/Makefile
utils_lgpl/nefis/tests/test_03/Makefile
utils_lgpl/nefis/tests/test_04/Makefile
utils_lgpl/nefis/tests/test_05/Makefile
utils_lgpl/nefis/tests/test_06/Makefile
utils_lgpl/nefis/tests/test_07/Makefile
utils_lgpl/nefis/tests/test_08/Makefile
utils_lgpl/nefis/tests/test_09/Makefile
utils_lgpl/nefis/tests/test_10/Makefile
utils_lgpl/nefis/tests/test_11/Makefile
utils_lgpl/nefis/tests/test_12/Makefile
utils_lgpl/nefis/tests/test_13/Makefile
utils_lgpl/nefis/tests/test_14/Makefile
utils_lgpl/nefis/tests/test_15/Makefile
utils_lgpl/nefis/tests/test_16/Makefile
utils_lgpl/nefis/tests/test_17/Makefile
utils_lgpl/nefis/tests/test_18/Makefile
utils_lgpl/nefis/tests/test_19/Makefile
utils_lgpl/nefis/tests/test_20/Makefile
utils_lgpl/nefis/tests/test_21/Makefile
utils_lgpl/ods/Makefile
utils_lgpl/ods/packages/Makefile
utils_lgpl/ods/packages/ods/Makefile
utils_lgpl/ods/packages/ods/srcf/Makefile
utils_lgpl/ods/packages/ods/src/Makefile

])

AC_CONFIG_SUBDIRS([
    third_party_open/kdtree2
    third_party_open/libsigwatch
])

if test "x$with_shapelib" != "xno" -o "x$with_proj" != "xno"; then
AC_CONFIG_SUBDIRS([
    third_party_open/fortrangis
])
fi

AC_OUTPUT
