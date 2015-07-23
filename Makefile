#DEBUG := --ghc-options="-ddump-splices"

ICON := icon

APP := dist/build/planar.app
BIN := $(APP)/Contents/MacOS/planar

DEP := *.hs Info.plist $(ICON).icns

$(APP) : $(DEP)
	cabal build $(DEBUG) -j1 -v0
#	cabal build $(DEBUG) -j8 -v0

run: $(APP)
	./dist/build/planar.app/Contents/MacOS/planar

open: $(APP)
	open ./dist/build/planar.app

I := $(ICON).iconset/$(ICON)_
eq = $(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))
double = $(if $(call eq,$(1),512x512),1024x1024,\
         $(if $(call eq,$(1),256x256),512x512,\
         $(if $(call eq,$(1),128x128),256x256,\
	 $(if $(call eq,$(1),64x64),128x128,\
	 $(if $(call eq,$(1),32x32),64x64,\
         $(if $(call eq,$(1),16x16),32x32,"fail"))))))

$(I)%@2x.png: icon_512x512.png
	convert -resize $(call double,$*) $(ICON)_512x512.png $(I)$*@2x.png
$(I)%.png: icon_512x512.png
	convert -resize $* $(ICON)_512x512.png $(I)$*.png

$(ICON).iconset: $(foreach D,16x16 32x32 128x128 256x256 512x512,$(I)$(D).png $(I)$(D)@2x.png)

$(ICON).icns: $(ICON).iconset
	iconutil -c icns $(ICON).iconset

Info.plist: Info.xml
	plutil -convert xml1 Info.xml -o Info.plist

clean:
	cabal clean
	rm -Rf Info.plist
	rm -Rf $(ICON).icns
	rm -Rf $(ICON).iconset/*
