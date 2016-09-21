dnl
dnl  Configure script for sigwatcher
dnl
dnl  Process this file with autoconf to generate the configure script

AC_INIT(libsigwatch, 1.0)
AC_SUBST(RELEASE_DATE, '2011 February 2')
AM_INIT_AUTOMAKE

AC_CONFIG_MACRO_DIR([m4])

AC_PROG_CC
AC_PROG_F77
AC_PROG_LIBTOOL

AC_CONFIG_SRCDIR([sigwatch.c])

dnl   Disable automake clever-clever invocation of autoheader.
dnl   cf <http://mail.gnu.org/archive/html/automake/2003-01/msg00051.html>
dnl AUTOHEADER="echo autoheader ignored"

AC_CHECK_HEADERS([signal.h])
AC_MSG_CHECKING(whether NSIG is defined)
AC_EGREP_CPP([HAVE *[0-9][0-9]*],
                [#include <signal.h>
                 HAVE NSIG],
                AC_DEFINE(HAVE_NSIG,1,[Have NSIG defined])
		AC_MSG_RESULT(yes),
		AC_MSG_RESULT(no))

if test -n "$F77"; then
AC_F77_WRAPPERS
fi

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile sigwatch.html])

AC_OUTPUT
