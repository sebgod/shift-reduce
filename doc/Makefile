include ../Make.options
include ../Project.options
# strange, on Windows wget, use -nc, on Ubuntu, use -N
.PHONY: default
default:
	$(WGET) -r -N -k -l 2 http://$(PROJ_GP_DOMAIN)/ || true

.PHONY: clean
clean:

.PHONY: realclean
realclean:
	rm -fR $(PROJ_GP_DOMAIN)
