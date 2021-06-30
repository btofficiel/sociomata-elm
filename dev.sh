bash ./optimize.sh src/Main.elm
mv elm.min.js ../sociomata-static/public/js/
rm -rf elm.min.js elm.js

