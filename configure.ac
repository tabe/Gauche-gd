dnl
dnl Configuring Gauche-gd
dnl  process this file with autoconf to generate 'configure'.
dnl $Id$
dnl

AC_PREREQ(2.54)
AC_INIT(Gauche-gd, 0.1.0, tabe@fixedpoint.jp)
dnl If you want to use the system name (OS, architecture, etc) in the
dnl configure, uncomment the following line.  In such a case, you need
dnl to copy config.guess and config.sub from automake distribution.
dnl AC_CANONICAL_SYSTEM

dnl Set up gauche related commands.  The commands are set by scanning
dnl PATH.  You can override them by "GOSH=/my/gosh ./configure" etc.
AC_PATH_PROG([GOSH], gosh)
AC_PATH_PROG([GAUCHE_CONFIG], gauche-config)
AC_PATH_PROG([GAUCHE_PACKAGE], gauche-package)
AC_PATH_PROG([GAUCHE_INSTALL], gauche-install)
AC_PATH_PROG([GAUCHE_CESCONV], gauche-cesconv)

dnl Usually these parameters are set by AC_PROG_CC, but we'd rather use
dnl the same one as Gauche has been compiled with.
SOEXT=`$GAUCHE_CONFIG --so-suffix`
OBJEXT=`$GAUCHE_CONFIG --object-suffix`
EXEEXT=`$GAUCHE_CONFIG --executable-suffix`
AC_SUBST(SOEXT)
AC_SUBST(OBJEXT)
AC_SUBST(EXEEXT)

dnl Check for headers.
dnl Add your macro calls to check required headers, if you have any.
AC_CHECK_HEADER(gd.h, [], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER(gdfontg.h, [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTG_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER(gdfontl.h, [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTL_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER(gdfontmb.h, [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTMB_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER(gdfonts.h, [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTS_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER(gdfontt.h, [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTT_H"], [AC_MSG_RESULT([could not found])])
AC_SUBST(GDFONT_CPPFLAGS)

dnl Check for other programs.
dnl Add your macro calls to check existence of programs, if you have any.
AC_PROG_CC
AC_PROG_CPP

AC_PATH_PROGS(GDLIB_CONFIG, gdlib-config)
AC_SUBST(GDLIB_CONFIG)
if test "x${GDLIB_CONFIG}" = "x"; then
  GDLIB_CPPFLAGS="-DGD_GIF"
  AC_SUBST(GDLIB_CPPFLAGS)
else
  GDLIB_CFLAGS=`${GDLIB_CONFIG} --cflags`
  AC_SUBST(GDLIB_CFLAGS)
  GDLIB_LDFLAGS=`${GDLIB_CONFIG} --ldflags`
  AC_SUBST(GDLIB_LDFLAGS)
  GDLIB_LIBS=`${GDLIB_CONFIG} --libs`
  AC_SUBST(GDLIB_LIBS)
  for f in `${GDLIB_CONFIG} --features`; do
    GDLIB_CPPFLAGS="${GDLIB_CPPFLAGS} -D$f"
  done
  AC_SUBST(GDLIB_CPPFLAGS)
fi

dnl Check for libraries
dnl Add your macro calls to check required libraries, if you have any.

dnl Creating gpd (gauche package description) file
GAUCHE_PACKAGE_CONFIGURE_ARGS="`echo ""$ac_configure_args"" | sed 's/[\\""\`\$]/\\\&/g'`"
AC_MSG_NOTICE([creating ${PACKAGE_NAME}.gpd])
$GAUCHE_PACKAGE make-gpd "$PACKAGE_NAME" \
  -version "$PACKAGE_VERSION" \
  -configure "./configure $GAUCHE_PACKAGE_CONFIGURE_ARGS"

dnl Output
echo -n $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile doc/Makefile example/Makefile)
