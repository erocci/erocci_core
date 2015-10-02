dnl
dnl Autoconf initialization
dnl
AC_PREREQ([2.69])
AC_INIT([erocci_core],
	m4_esyscmd([(git describe --always --tags 2>/dev/null || echo 0.6) | sed 's/^v//' |
        sed 's/-\([0-9]\)-/-0\1-/' | tr -d '\n' || echo 1]),
	[http://github.com/erocci/erocci/issues],
        [erocci],
        [https://erocci.ow2.org])
AC_CONFIG_SRCDIR([src/erocci_core.app.src])
if test -d "$srcdir/deps/erim_xml"; then
  AC_CONFIG_SUBDIRS([deps/erim_xml])
fi

AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

dnl ------------------------------------------------------------------
dnl Versioning.
dnl ------------------------------------------------------------------

dnl Is this a final release?
is_release=0
GITDESC1=`git describe --abbrev=0 || echo 1`
GITDESC2=`git describe --abbrev=4 || echo 1`
if test "${GITDESC1}" = "${GITDESC2}"; then
       is_release=1
fi

AC_CONFIG_FILES([
	Makefile
])

AC_OUTPUT
