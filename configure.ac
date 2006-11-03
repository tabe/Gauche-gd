dnl
dnl Configuring Gauche-gd
dnl  process this file with autoconf to generate 'configure'.
dnl $Id$
dnl

AC_PREREQ(2.54)
AC_INIT(Gauche-gd, 0.1.1, tabe@fixedpoint.jp)
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
AC_CHECK_HEADER([gd.h], [], [AC_MSG_ERROR([could not found and exit])])
AC_CHECK_HEADER([gdfontg.h], [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTG_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER([gdfontl.h], [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTL_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER([gdfontmb.h], [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTMB_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER([gdfonts.h], [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTS_H"], [AC_MSG_RESULT([could not found])])
AC_CHECK_HEADER([gdfontt.h], [GDFONT_CPPFLAGS="${GDFONT_CPPFLAGS} -DHAVE_GDFONTT_H"], [AC_MSG_RESULT([could not found])])
AC_SUBST(GDFONT_CPPFLAGS)

dnl Check for other programs.
dnl Add your macro calls to check existence of programs, if you have any.
AC_PROG_CC
AC_PROG_CPP
AC_PROG_EGREP
AC_PATH_PROGS(GDLIB_CONFIG, gdlib-config)

dnl Check for libraries
dnl Add your macro calls to check required libraries, if you have any.
AC_EGREP_HEADER([gdImageOpenPolygon], [gd.h], [GDLIB_CPPFLAGS="${GDLIB_CPPFLAGS} -DWITH_GD_IMAGE_OPEN_POLYGON"], [AC_MSG_RESULT([could not found gdImageOpenPolygon])])
AC_EGREP_HEADER([gdFTUseFontConfig],  [gd.h], [GDLIB_CPPFLAGS="${GDLIB_CPPFLAGS} -DWITH_GD_FT_USE_FONT_CONFIG"], [AC_MSG_RESULT([could not found gdFTUseFontConfig])])

if test "x${GDLIB_CONFIG}" = "x"; then
  GDLIB_VERSION=unknown
  GDLIB_CPPFLAGS="-DGD_GIF"
else
  GDLIB_VERSION=`${GDLIB_CONFIG} --version`
  GDLIB_CPPFLAGS="${GDLIB_CPPFLAGS} -DGDLIB_VERSION=\"\\\"${GDLIB_VERSION}\\\"\""
  GDLIB_CFLAGS=`${GDLIB_CONFIG} --cflags`
  GDLIB_LDFLAGS=`${GDLIB_CONFIG} --ldflags`
  GDLIB_LIBS=`${GDLIB_CONFIG} --libs`
  for f in `${GDLIB_CONFIG} --features`; do
    GDLIB_CPPFLAGS="${GDLIB_CPPFLAGS} -D$f"
  done
fi

AC_SUBST(GDLIB_CPPFLAGS)
AC_SUBST(GDLIB_CFLAGS)
AC_SUBST(GDLIB_LDFLAGS)
AC_SUBST(GDLIB_LIBS)

dnl Creating gpd (gauche package description) file
GAUCHE_PACKAGE_CONFIGURE_ARGS="`echo ""$ac_configure_args"" | sed 's/[\\""\`\$]/\\\&/g'`"
AC_MSG_NOTICE([creating ${PACKAGE_NAME}.gpd])
$GAUCHE_PACKAGE make-gpd "$PACKAGE_NAME" \
  -version "$PACKAGE_VERSION" \
  -configure "./configure $GAUCHE_PACKAGE_CONFIGURE_ARGS"

dnl Output
echo -n $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile doc/Makefile example/Makefile)
