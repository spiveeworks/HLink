#           #
# Variables #
#           #

_HLINK=mark mount join
_MISC=compressPaths

HLINK=$(_HLINK:%,binaries/%)
MISC=$(_MISC:%,binaries/%)


#                  #
# Production Rules #
#                  #

# Useful Names

all: hLink hLinkAlias misc

hLinkAlias: $(HLINK)
misc: $(MISC)

# Production

$(HLINK) $(MISC): Alias.hs
	ghc -outputdir output/alias -o $@ --make Alias.hs -main-is $@F

hLink:
	ghc -outputdir output -o binaries/hLink --make Main.hs


#            #
# make stuff #
#            #

.PHONY: clean all hLink hLinkAlias misc

clean:
	rm -f $(ODIR)/* *~ core $(INCDIR)/*~

