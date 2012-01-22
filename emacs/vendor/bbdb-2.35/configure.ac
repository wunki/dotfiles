dnl configure.ac --- configuration setup for BBDB

dnl Author:        Didier Verna <didier@xemacs.org>
dnl Maintainer:    Didier Verna <didier@xemacs.org>, Waider <waider@waider.ie>
dnl Created:       Tue Nov 14 18:28:52 2000
dnl Last Revision: $Date: 2006/01/29 20:28:31 $

dnl Copyright (C) 2000-2001 Didier Verna

dnl BBDB is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU Library General Public License as published
dnl by the Free Software Foundation; either version 2 of the License, or (at
dnl your option) any later version.

dnl BBDB is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU Library General Public License for more details.

dnl You should have received a copy of the GNU Library General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


dnl Process this file with autoconf to produce a new configure script

AC_PREREQ([2.50])

BBDB_PRE_INIT
AC_INIT([BBDB], [BBDB_VERSION], [bbdb-info@lists.sourceforge.net])

AC_COPYRIGHT([Copyright (C) 2000-2001 Didier Verna <didier@xemacs.org>.])
AC_REVISION([$Revision: 2.11 $])

AC_CONFIG_SRCDIR([configure.ac])
dnl We only have install-sh right now, so it's not worth adding an aux
dnl directory just for that file.
dnl AC_ CONFIG_AUX_DIR
dnl The following macro is incompatible with the current Automake, but we
dnl don't use it for now.
AC_CONFIG_FILES([Makefile
                 lisp/Makefile
                 texinfo/Makefile
                 tex/Makefile
                 utils/Makefile])

dnl Make sure we don't accidentally try to configure a testing directory if
dnl one doesn't exist.
if [[ -d testing ]]
then
  AC_CONFIG_FILES([testing/Makefile])
fi

dnl Common system utilities checking:
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(RM, rm, /bin/rm)
AC_PATH_PROG(GREP, grep, /bin/grep)

dnl ah ah ah.
AC_MSG_CHECKING([for my keys])
AC_MSG_RESULT([not found])
echo "Damn! I'm locked outside :-("

dnl External programs checking:
BBDB_PROG_GNU_TAR
BBDB_PROG_COMPRESS
BBDB_PROG_MAKEINFO
BBDB_PROG_TEXI2DVI
BBDB_PROG_ETAGS
BBDB_PROG_EMACS

dnl Some customizations:
BBDB_ARG_SUBST([GNUSDIR], [gnus-dir], [DIR], [set the location of Gnus to DIR],
	           [], [[ enable_gnus=gnus ]])
BBDB_ARG_SUBST([MHEDIR],  [mhe-dir],  [DIR], [set the location of Mh-E to DIR],
	 		   [], [[ enable_mhe=mhe ]])
BBDB_ARG_SUBST([VMDIR],   [vm-dir],   [DIR], [set the location of VM   to DIR],
	           [], [[ enable_vm=vm ]])

# Various MTAs and other optional bits
# Default Gnus to on
enable_gnus=gnus

# this seems far messier and more complex than I'd expect from autoconf...
AC_ARG_ENABLE([vm],
              AC_HELP_STRING([--enable-vm],
                         [enable VM hooks (default depends on --with-vm-dir)]),
           [if test "x$enableval" = "xno"; then BBDB_VM=; enable_vm=; else BBDB_VM=vm; fi])

AC_ARG_ENABLE(rmail,
              AC_HELP_STRING([--enable-rmail],
                             [enable RMAIL hooks [[no]]]),
           [if test "x$enableval" = "xno"; then BBDB_RMAIL=; enable_rmail=; else BBDB_RMAIL=rmail; fi], [BBDB_RMAIL=])

AC_ARG_ENABLE(gnus,
			  AC_HELP_STRING([--enable-gnus],
                             [enable GNUS hooks [[yes]]]),
           [if test "x$enableval" = "xno"; then BBDB_GNUS=; enable_gnus=; else BBDB_GNUS=gnus; fi], [BBDB_GNUS=])

AC_ARG_ENABLE(mhe,
              AC_HELP_STRING([--enable-mhe],
                      [enable MH-E hooks (default depends on --with-mhe-dir)]),
              [if test "x$enableval" = "xno"; then BBDB_MHE=; enable_mhe=; else BBDB_MHE=mhe; fi], [BBDB_MHE=])

dnl All this does for now is allows you to see the compile commands
AC_ARG_ENABLE(developer,
			  AC_HELP_STRING([--enable-developer],
   [enable developer hooks [[no]]]),
   [if test "x$enable" = "xno"; then HUSHMAKE=; fi], [HUSHMAKE=@])

BBDB_ARG_SUBST([OTHERDIRS], [other-dirs], [DIRS],
  [set other needed directories (a list of space or colon separated paths)],
  [],
  dnl #### WARNING: pay attention to the quoting of ACTION if given !!!!!
  [[ BBDB_COLON_TO_SPACE(withval) ]])

dnl Set up load path file
echo "(setq load-path (append (list " > ${ac_confdir}/loadpath.el
for dir in . "${VMDIR}" "${GNUSDIR}" "${MHEDIR}" "${OTHERDIRS}"
do
  if test "x$dir" != "x"
  then
    echo "(expand-file-name \"$dir\") " >> ${ac_confdir}/loadpath.el
  fi
done
echo ") load-path))" >> ${ac_confdir}/loadpath.el

BBDB_LOADPATH="${ac_confdir}/loadpath.el"

dnl if VM was requested, make sure we have access to the source
if test "x$enable_vm" != "x"; then
   BBDB_VM=`${EMACS_PROG} --no-site-file --no-init-file -batch -q -l ${BBDB_LOADPATH} -eval "(if (locate-library \"vm-autoload\") (message \"vm\"))" 2>&1`
   if test "x$BBDB_VM" = "x"; then
	  AC_MSG_ERROR([*** Cannot build VM support without VM's source.])
   fi
fi

dnl Now go looking for auxilliary packages (supercite, reportmail and gnuserv)
BBDB_SC=`${EMACS_PROG} -no-site-file -no-init-file -batch -q -l ${BBDB_LOADPATH} -eval "(if (locate-library \"supercite\") (message \"bbdb-sc.elc\"))" 2>&1`
BBDB_REPORTMAIL=`${EMACS_PROG} -no-site-file -no-init-file -batch -q -l ${BBDB_LOADPATH} -eval "(if (locate-library \"reportmail\") (message \"bbdb-reportmail.elc\"))" 2>&1`
BBDB_SRV=`${EMACS_PROG} -no-site-file -no-init-file -batch -q -l ${BBDB_LOADPATH} -eval "(progn (or (fboundp (quote define-obsolete-variable-alias)) (if (locate-library \"gnuserv-compat\") (load \"gnuserv-compat\" t t))) (if (and (locate-library \"gnuserv\") (locate-library \"itimer\")) (message \"bbdb-srv.el\")))" 2>&1`

dnl XEmacsery
dnl is there a sane way to set this to a useful default?
BBDB_ARG_SUBST([PACKAGEDIR], [package-dir], [DIR],
  [set the XEmacs package directory to DIR],
  [/usr/lib/xemacs/site-packages])

BBDB_ARG_SUBST([SYMLINKS], [symlinks], [],
  [install BBDB by linking instead of copying], [no])

BBDB_ARG_SUBST([LINKPATH], [linkpath], [PATH],
  [path to symlink from if `pwd' does not work])

# Switch on some targets
AC_SUBST([BBDB_RMAIL])
AC_SUBST([BBDB_VM])
AC_SUBST([BBDB_GNUS])
AC_SUBST([BBDB_MHE])
AC_SUBST([BBDB_LOADPATH])
AC_SUBST([BBDB_SC])
AC_SUBST([BBDB_REPORTMAIL])
AC_SUBST([BBDB_SRV])
AC_SUBST([HUSHMAKE])

dnl Hacky Hacky. If RM really /is/ rm (i.e. we're not on DOS, for example)
dnl then make it 'quiet'.
if test "${RM}" = "/bin/rm" -o "${RM}" = "/usr/bin/rm"; then
  RM="${RM} -f"
fi

dnl More Hacky. Figure out the correct way to get contextual grep.
dnl this should only be switched on for developers since it's
dnl only required by the test stuff.
if test "x$enable_developer" != "x"; then
  if ${GREP} -C foo configure.ac > /dev/null 2>&1; then
     GREPCONTEXT="-C"
  else
     GREPCONTEXT="-C5"
  fi
fi

AC_SUBST(GREPCONTEXT)

AC_OUTPUT

dnl configure.ac ends here
