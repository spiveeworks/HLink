#           #
# Variables #
#           #

HLINK=mark mount join pull block
MISC=compressPaths

#                  #
# Production Rules #
#                  #

# Useful Names

all: hLink hLinkAlias misc

hLinkAlias: $(HLINK)
misc: $(MISC)

# Production

$(HLINK) $(MISC): Alias.hs
	ghc -outputdir output/alias -o binaries/$@ --make Alias.hs -main-is $@

hLink:
	ghc -outputdir output -o binaries/hLink --make Main.hs


#            #
# make stuff #
#            #

.PHONY: clean all hLink hLinkAlias misc $(HLINK) $(MISC)

clean:
	rm -f $(ODIR)/* *~ core $(INCDIR)/*~

