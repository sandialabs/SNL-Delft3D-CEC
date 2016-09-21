dnl check to see if we can write to binary output
AC_DEFUN([ACX_FC_FORM_BINARY],
 [AC_CACHE_CHECK([whether $FC supports writing in form "binary" features],
 [ax_cv_fc_supports_form_binary],
 [AC_REQUIRE([AC_PROG_FC])
  AC_LANG([Fortran])
  AC_COMPILE_IFELSE([dnl
   program binaryformtest
     open(66,file='binaryformtest',form='binary')
   end program binaryformtest
 ],
 [ax_cv_fc_supports_form_binary="yes"],
 [ax_cv_fc_supports_form_binary="no"])
])
if test x"$ax_cv_fc_supports_form_binary" = xyes; then
AC_DEFINE([FC_FORM_BINARY], 1,
[Define if Fortran supports open(...,form="binary").])
fi])

dnl check to see if Fortran compiler support the shared specifier in open statelemt
AC_DEFUN([ACX_FC_SHARED_SPECIFIER],
 [AC_CACHE_CHECK([whether $FC supports shared specifier in open statement],
 [ax_cv_fc_supports_shared_specifier],
 [AC_REQUIRE([AC_PROG_FC])
  AC_LANG([Fortran])
  AC_COMPILE_IFELSE([dnl
   program sharedtest
     open(66,file='foobar',shared)
   end program sharedtest
 ],
 [ax_cv_fc_supports_shared_specifier="yes"],
 [ax_cv_fc_supports_shared_specifier="no"])
])
if test x"$ax_cv_fc_supports_shared_specifier" = xyes; then
AC_DEFINE([FC_SHARED_SPECIFIER], 1,
[Define if Fortran supports open(...,shared).])
fi])


dnl check to see if Fortran 77 compiler support the shared specifier in open statelemt
AC_DEFUN([ACX_F77_SHARED_SPECIFIER],
[AC_CACHE_CHECK([whether $F77 supports shared specifier in open statement],
[ax_cv_f77_supports_shared_specifier],
[AC_REQUIRE([AC_PROG_F77])
AC_LANG([Fortran 77])
AC_COMPILE_IFELSE([dnl
program sharedtest
open(66,file='foobar',shared)
end program sharedtest
],
[ax_cv_f77_supports_shared_specifier="yes"],
[ax_cv_f77_supports_shared_specifier="no"])
])
if test x"$ax_cv_f77_supports_shared_specifier" = xyes; then
AC_DEFINE([F77_SHARED_SPECIFIER], 1,
[Define if Fortran 77 supports open(...,shared).])
fi])
