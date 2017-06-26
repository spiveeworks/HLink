HLINK=mark mount
MISC=compressPaths


all: hlink misc

hlink: $(HLINK)
misc: $(MISC)

$(HLINK) $(MISC):
	ghc -outputdir output -o binaries/$@ --make Main.hs -main-is $@


#       #
# HLink #
#       #



#            #
# make stuff #
#            #

.PHONY: clean all hlink misc $(HLINK) $(MISC)  # lol everything

clean:
	rm -f $(ODIR)/* *~ core $(INCDIR)/*~
