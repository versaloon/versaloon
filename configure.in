AC_PREREQ(2.59)
AC_INIT([vsprog], [0.0.1], 
  [VSProg Mailling List <Versaloon-development@simonqian.com>])
AC_CONFIG_SRCDIR([src/vsprog.c])

AM_INIT_AUTOMAKE(vsprog, 1.0)
AM_MAINTAINER_MODE

AM_CONFIG_HEADER(config.h)

AC_LANG_C
AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_CC_C_O
AC_PROG_RANLIB

dnl disable checks for C++, Fortran and GNU Java Compiler
m4_defun([_LT_AC_LANG_CXX_CONFIG], [:])
m4_defun([_LT_AC_LANG_F77_CONFIG], [:])
m4_defun([_LT_AC_LANG_GCJ_CONFIG], [:])

AC_CANONICAL_HOST

AM_PATH_XML2(2.4.0,,
  AC_MSG_ERROR([Libxml2 is required to build vsprog]))

AC_CHECK_HEADERS(malloc.h)
AC_CHECK_HEADERS(sys/time.h)
AC_CHECK_HEADERS(strings.h)
AC_CHECK_HEADERS(unistd.h)

AC_HEADER_ASSERT
AC_HEADER_STDBOOL
AC_HEADER_TIME

AC_C_BIGENDIAN

AC_CHECK_FUNCS(usleep)
AC_CHECK_FUNCS(strtoul)
AC_CHECK_FUNCS(gettimeofday)
AC_CHECK_FUNCS(strchr)

is_cygwin=no
is_mingw=no
is_win32=no
is_darwin=no
build_release=yes

AC_ARG_ENABLE(release,
  AS_HELP_STRING([--enable-release], [Enable Release Build, default no]),
  [build_release=$enableval], [build_release=no])

if test $cross_compiling = no; then
  if test $build_release = no; then
    # check we can find guess-rev.sh
    AC_CHECK_FILE("$srcdir/guess-rev.sh", build_release=no, build_release=yes)
  fi
fi

case $host in 
  *-darwin*)
    is_darwin=yes
	
	AC_DEFINE(IS_DARWIN, 1, [1 if building for Darwin.])
	;;
  *-cygwin*) 
    is_win32=yes

    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([],[return __MINGW32__;]),
      [is_mingw=yes],[is_mingw=no])
    if test $is_mingw = yes; then
      AC_DEFINE(IS_MINGW, 1, [1 if building for MinGW.])
      is_cygwin=no
    else
      is_cygwin=yes
      AC_DEFINE(IS_CYGWIN, 1, [1 if building for Cygwin.])
    fi

    AC_DEFINE(IS_WIN32, 1, [1 if building for Win32.])
    ;; 
  *-mingw*) 
    is_mingw=yes
    is_win32=yes

    AC_DEFINE(IS_MINGW, 1, [1 if building for MinGW.])
    AC_DEFINE(IS_WIN32, 1, [1 if building for Win32.])
    ;; 
  *) 
    AC_DEFINE(IS_CYGWIN, 0, [0 if not building for Cygwin.])
    AC_DEFINE(IS_WIN32, 0, [0 if not building for Win32.])
	AC_DEFINE(IS_DARWIN, 0, [0 if not building for Darwin.])
    ;;
esac

AM_CONDITIONAL(RELEASE, test $build_release = yes)
AM_CONDITIONAL(IS_CYGWIN, test $is_cygwin = yes)
AM_CONDITIONAL(IS_MINGW, test $is_mingw = yes)
AM_CONDITIONAL(IS_WIN32, test $is_win32 = yes)

GCC_WARNINGS="-Wall -Wstrict-prototypes -Wformat-security"
GCC_WARNINGS="${GCC_WARNINGS} -Wno-unused-parameter -Wno-missing-field-initializers"
GCC_WARNINGS="${GCC_WARNINGS} -Werror -Wextra"
GCC_WARNINGS="${GCC_WARNINGS} -Wbad-function-cast"
GCC_WARNINGS="${GCC_WARNINGS} -Wcast-align"
GCC_WARNINGS="${GCC_WARNINGS} -Wredundant-decls"
CFLAGS="$CFLAGS $GCC_WARNINGS $XML_CPPFLAGS"

AC_MSG_CHECKING([for suffix of executable build tools])
if test $cross_compiling = yes; then
  cat >conftest.c <<\_______EOF
int main ()
{
  exit (0);
}
_______EOF
  for i in .exe ""; do
    compile="$CC_FOR_BUILD conftest.c -o conftest$i"
    if AC_TRY_EVAL(compile); then
      if (./conftest) 2>&AC_FD_CC; then
        EXEEXT_FOR_BUILD=$i
        break
        fi
      fi
  done
  rm -f conftest*
  if test "${EXEEXT_FOR_BUILD+set}" != set; then
    AC_MSG_ERROR([Cannot determine suffix of executable build tools])
  fi
else
	EXEEXT_FOR_BUILD=$EXEEXT
fi
AC_MSG_RESULT([$EXEEXT_FOR_BUILD])
AC_SUBST(EXEEXT_FOR_BUILD)

AC_OUTPUT(
    Makefile
    doc/Makefile
    src/Makefile
    src/driver/Makefile
    src/target/Makefile
    src/tools/Makefile
    src/programmer/Makefile
    vsf/Makefile
    src/scripts/Makefile
  )
