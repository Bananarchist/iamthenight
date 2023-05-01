#!/bin/sh

# declarations
output_dir="dist"
entry_dir="src"
debug=false
dist=false

for arg in $@; do
	if [[ $arg =~ "debug" ]]; then
		debug=true
	elif [[ $arg =~ "dist" ]]; then
		dist=true
	elif [[ $arg =~ "editor" ]]; then
		entry_dir="src/Editor"
		output_dir="dist/editor"
	else
		echo "Unexpected argument $arg"
	fi
done

# create output directories
mkdir -p $output_dir
mkdir -p $output_dir/assets
mkdir -p $output_dir/levels

js_out="$output_dir/main.js"
entry="$entry_dir/Main.elm"
# compile elm code
if $debug; then
	echo "Building for debug"
	elm make $entry --output=$output_dir/main.js --debug
else
	echo "Building for prod"
	elm make $entry --output=$js_out
fi

# copy resources to dist
cp $entry_dir/index.html $output_dir
cp $entry_dir/index.css $output_dir
cp assets/*.svg dist
cp assets/levels/*.json dist/levels

if $dist; then
	echo "Compressing client for distribution"
	zip -r client.zip $output_dir
fi
