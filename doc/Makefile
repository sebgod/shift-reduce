include ../Make.options
include ../Project.options

# we should check if we have to use -nc instead of -N,
# which is required for some [which?] versions of wget
.PHONY: default
default:
	$(WGET) -P orig -nv -r -N -l 2 http://$(PROJ_GP_DOMAIN)/ || true
	cp -r -u orig/$(PROJ_GP_DOMAIN) $(PROJ_GP_DOMAIN)
	$(WGET) -nv -r -nc -k -l 2 http://$(PROJ_GP_DOMAIN)/ || true

.PHONY: zip
zip: $(PROJ_GP_DOMAIN).$(ZIP_EXT)

$(PROJ_GP_DOMAIN).$(ZIP_EXT): $(PROJ_GP_DOMAIN)
	$(ZIP) $(ZIP_UFLAGS) $@ $(PROJ_GP_DOMAIN)

.PHONY: clean
clean:
	rm -fR $(PROJ_GP_DOMAIN)

.PHONY: realclean
realclean:
	rm -fR orig
