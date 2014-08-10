# not a GNU package. You can remove this line, if
# have all needed files, that a GNU package needs

SUBDIRS = src doc

CFG_PATH = src/config
CFG_FILES = find $(srcdir)/$(CFG_PATH) -name '*.xml' | \
		sed -e 's,^$(srcdir)/$(CFG_PATH),,'

dist-hook:
	for i in $$($(CFG_FILES)); do \
		j="$(distdir)/$(CFG_FILES)/$$i" && \
		mkdir -p "$$(dirname $$j)" && \
		$(INSTALL_DATA) $(srcdir)/$(CFG_PATH)/$$i $$j; \
	done

install-data-hook:
	for i in $$($(CFG_FILES)); do \
		j="$(DESTDIR)$(pkgdatadir)/config/$$i" && \
		mkdir -p "$$(dirname $$j)" && \
		$(INSTALL_DATA) $(srcdir)/$(CFG_PATH)/$$i $$j; \
	done

uninstall-hook:
	rm -rf $(DESTDIR)$(pkgdatadir)/config

MAINTAINERCLEANFILES = \
	configure \
	Makefile.in \
	depcomp \
	config.guess \
	config.sub \
	config.h.in \
	missing \
	aclocal.m4 \
	install-sh \
	compile
