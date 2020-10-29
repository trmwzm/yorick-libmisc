# these values filled in by    yorick -batch make.i
Y_MAKEDIR=/home/trm/dev/yorick/yorick/relocate
Y_EXE=/home/trm/dev/yorick/yorick/relocate/bin/yorick
Y_EXE_PKGS=
Y_EXE_HOME=/home/trm/dev/yorick/yorick/relocate
Y_EXE_SITE=/home/trm/dev/yorick/yorick/relocate
Y_HOME_PKG=

#
# !! THIS IS NOT A PLUGIN !!
# This is a package made of several interpreted
# include file. This makefile is just used to install,
# uninstall it or build the distribution tar file.

# ------------------------------------------------ macros for this package

# used for distribution
PKG_NAME = libmisc
# include files for this package
PKG_I= ascii.i convcorrel.i dbase.i dsp.i fcomplex.i fmin.i fzero.i img.i \
       linalg.i memfmap.i pocs.i rdf.i rotation.i density.i \
       shellargs.i xplot0.i yut.i bicub.i coreg.i digfun.i fitpoly.i \
       fpanels.i imbinavrg.i inpolygon.i lmfit.i \
       plot.i poly_fit.i rdf-refac.i rwflat.i time.i xplot.i gaupro.i \
       nninterp2.i histop.i moveop.i utils.i \
       prstruct.i rdcols.i plcm.i plim.i levmar_tools.i scm.i json.i
# autoload file for this package, if any
PKG_I_START = libmisc-start.i

# override macros Makepkg sets for rules and other macros
# Y_HOME and Y_SITE in Make.cfg may not be correct (e.g.- relocatable)
Y_HOME=$(Y_EXE_HOME)
Y_SITE=$(Y_EXE_SITE)

# include $(Y_MAKEDIR)/Make.cfg
DEST_Y_SITE=$(DESTDIR)$(Y_SITE)
DEST_Y_HOME=$(DESTDIR)$(Y_HOME)

# ------------------------------------- targets and rules for this package

build:
	@echo "Nothing to build. This is not a plugin"
	@echo "other targets: install, uninstall, clean"
	@echo "for maintainers: package, distpkg"

clean:
	-rm -rf pkg *~

install:
	mkdir -p $(DEST_Y_SITE)/i
	cp -p $(PKG_I) $(DEST_Y_SITE)/i/
	cp -p $(PKG_I_START) $(DEST_Y_SITE)/i-start/

uninstall:
	-cd $(DEST_Y_SITE)/i; rm $(PKG_I)
	-rm $(DEST_Y_SITE)/i-start/libmisc-start.i

# -------------------------------------------- package build rules

PKG_VERSION = $(shell (awk '{if ($$1=="Version:") print $$2}' $(PKG_NAME).info))
# .info might not exist, in which case he line above will exit in error.

package:
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/i
	cp -p $(PKG_I) pkg/$(PKG_NAME)/dist/y_site/i/
	cd pkg/$(PKG_NAME)/dist/y_site/i/; if test -f "check.i"; then rm check.i; fi
	if test -f "check.i"; then cp -p check.i pkg/$(PKG_NAME)/.; fi
	if test -n "$(PKG_I_START)"; then cp -p $(PKG_I_START) \
	  pkg/$(PKG_NAME)/dist/y_site/i-start/; fi
	cp -p $(PKG_NAME).info pkg/$(PKG_NAME)/$(PKG_NAME).info
	cd pkg; tar zcvf $(PKG_NAME)-$(PKG_VERSION)-pkg.tgz $(PKG_NAME)

distpkg:
#tarball there
	if test -f "pkg/$(PKG_NAME)-$(PKG_VERSION)-pkg.tgz" ; then \
	  ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/tarballs/ \
	  pkg/$(PKG_NAME)-$(PKG_VERSION)-pkg.tgz; fi
#info files in each architecture directory
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/darwin-ppc/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/darwin-i386/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/linux-ppc/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/linux-x86/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi

distsrc:
	make clean
	-rm -rf pkg
	cd ..; tar --exclude pkg --exclude .svn --exclude CVS --exclude *.spec -zcvf \
	   $(PKG_NAME)-$(PKG_VERSION)-src.tgz yorick-$(PKG_NAME)-$(PKG_VERSION);\
	ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/src/ \
	   $(PKG_NAME)-$(PKG_VERSION)-src.tgz
	ncftpput -f $(HOME)/.ncftp/maumae www/yorick/contrib/ \
	   ../$(PKG_NAME)-$(PKG_VERSION)-src.tgz


# -------------------------------------------------------- end of Makefile
