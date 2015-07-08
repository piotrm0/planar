#DEBUG := --ghc-options="-ddump-splices"

build: Info.plist
	cabal build -j8 -v0 $(DEBUG)

run: Info.plist
	make build
	./dist/build/planar.app/Contents/MacOS/planar

open: Info.plist
	make build
	open ./dist/build/planar.app

planar.iconset: icon/icon_512x512.png

I := icon.iconset/icon_
eq = $(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))
double = $(if $(call eq,$(1),512x512),1024x1024,\
         $(if $(call eq,$(1),256x256),512x512,\
         $(if $(call eq,$(1),128x128),256x256,\
	 $(if $(call eq,$(1),64x64),128x128,\
	 $(if $(call eq,$(1),32x32),64x64,\
         $(if $(call eq,$(1),16x16),32x32,"fail"))))))

$(I)%@2x.png: icon_512x512.png
	convert -resize $(call double,$*) icon_512x512.png $(I)$*@2x.png
$(I)%.png: icon_512x512.png
	convert -resize $* icon_512x512.png $(I)$*.png

icon.iconset: $(foreach D,16x16 32x32 128x128 256x256 512x512,$(I)$(D).png $(I)$(D)@2x.png)

icon.icns: icon.iconset
	iconutil -c icns icon.iconset

icon: icon.icns

Info.plist: Info.xml
	plutil -convert xml1 Info.xml -o Info.plist

clean:
	cabal clean
	rm -Rf Info.plist
	rm -Rf icon.icns
	rm -Rf icon.iconset/*
