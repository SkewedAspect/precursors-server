#! /bin/sh

function pre_compile {
	if [ ! -d ebin ]; then
		mkdir ebin
	fi
	
	for file in proto_src/*.proto
	do
		nameBase=`echo "$file" | sed -e "s/^proto_src\///"`
		nameBase="src/${nameBase}"
		if [ ! -e $nameBase -o $file -nt $nameBase ]
		then
			cp $file src/
		fi
	done
	
	# hack for reltool
	if [ ! -d precursors_server ]; then
		mkdir precursors_server
		ln -sf ../ebin precursors_server/ebin
		ln -sf ../src precursors_server/src
		ln -sf ../include precursors_server/include
		ln -sf ../priv precursors_server/priv
		ln -sf ../deps precursors_server/deps
	fi
	
	# record what commit/version openacd is at
	PRECURSORS_COMMIT=""
	if [ -d ".hg" ]
	then
		PRECURSORS_COMMIT=`hg log -r tip --template '{node}'`
	fi
	if [ -e "include/commit_ver.hrl" ] && [ ! $PRECURSORS_COMMIT ]
	then
		exit 0
	else
		if [ ! $PRECURSORS_COMMIT ]
		then
			PRECURSORS_COMMIT="undefined"
		else
			PRECURSORS_COMMIT="\"$PRECURSORS_COMMIT\""
		fi
	fi
	echo "
%% automatically generated by precursors_server precompile script.  Editing
%% means it will just get overwritten again.

-define(PRECURSORS_COMMIT, $PRECURSORS_COMMIT)." > include/commit_ver.hrl
}

function post_compile {
	cat succes_message
}

case $1 in
	"pre_compile")
		pre_compile;;
	"post_compile")
		post_compile;;
	"pre_clean")
		rm -rf precursors_server
		rm include/commit_ver.hrl;;
esac
