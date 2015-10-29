
PLUGIN_PATH=/Users/Hoglund/Library/Application\ Support/Avid/Sibelius\ 7/Plugins
APP_NAME="Sibelius 7"

# EXAMPLE=~/Documents/Musik/Konserter/Flöjt/*.sib
#EXAMPLE=~/Documents/Musik/Etyder/Kontrapunkt/Fuga\ 106.sib
EXAMPLE=test.sib

install-plugin:
	iconv -f UTF-8 -t UTF-16LE <ExportJSON.plgx >ExportJSON.plgy

	pushd util && runhaskell MakeBOM.hs && popd
	cat util/LE.bom ExportJSON.plgy >ExportJSON.plg

	rm ExportJSON.plgy
	mkdir -p $(PLUGIN_PATH)/JSON/
	mv ExportJSON.plg $(PLUGIN_PATH)/JSON/ExportJSON.plg

run-sibelius: install-plugin
	killall $(APP_NAME)
	open -a $(APP_NAME) #$(EXAMPLE)


