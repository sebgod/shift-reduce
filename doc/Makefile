include ../Make.options
include ../Project.options

.PHONY: default
default: $(PROJ_GP_DOMAIN)

$(PROJ_GP_DOMAIN):
	$(WGET) -N -m http://$(PROJ_GP_DOMAIN)/ || true

.PHONY: clean
clean:

.PHONY: realclean
realclean:
	rm -fR $(PROJ_GP_DOMAIN)
