
PLUGIN_PATH=~/Library/Application\ Support/Sibelius\ Software/Sibelius\ 6/Plugins
PATH=$(PLUGIN_PATH)/JSON

test:
    cp ExportJSON.plg $(PATH)/ExportJSON.plg
    open -a /Applications/Sibelius\ 6.app

