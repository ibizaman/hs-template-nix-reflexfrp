#!/bin/bash

usage="USAGE: $0 PARENTPATH PROJECTNAME GITHUBUSER FULLNAME CACHIXHANDLE"

if [ -z "$1" ]; then
   echo "No PARENTPATH given, aborting"
   echo $usage
   exit 1
fi

projectpath="$1"
shift

if [ -z "$1" ]; then
   echo "No PROJECTNAME given, aborting"
   echo $usage
   exit 1
fi

projectname="$1"
shift

if [ -z "$1" ]; then
   echo "No GITHUBUSER given, aborting"
   echo $usage
   exit 1
fi

githubuser="$1"
shift

if [ -z "$1" ]; then
   echo "No FULLNAME given, aborting"
   echo $usage
   exit 1
fi

fullname="$1"
shift

if [ -z "$1" ]; then
   echo "No CACHIXHANDLE given, aborting"
   echo $usage
   exit 1
fi

cachixhandle="$1"
shift

function mkdirorexit () {
    if ! mkdir -p "$1"; then
    	echo "Could not create $1, aborting"
    	exit 1
    fi
}

fullpath="$projectpath/$projectname"

echo "Creating project in $fullpath"
echo "GITHUBUSER:    $githubuser"
echo "FULLNAME:      $fullname"
echo "CACHIXHANDLE:  $cachixhandle"

if [ -d "$fullpath" ]; then
    echo "$fullpath exists already, aborting"
	exit 1
fi
mkdirorexit "$fullpath"

cp ".gitignore"           "$fullpath"
cp "default.nix"          "$fullpath"
cp "cabal.project"        "$fullpath"
cp "cabal-ghcjs.project"  "$fullpath"
cp "hie.yaml"             "$fullpath"

cp "Makefile"             "$fullpath"
sed -i \
	-e s/ibizaman/"$cachixhandle"/g \
	"$fullpath/Makefile"

cp "modd.conf"            "$fullpath"

echo -e "# $projectname\nBased on template https://github.com/ibizaman/hs-template-nix-reflexfrp" \
      > "$fullpath/README.md"

cp "release.nix"          "$fullpath"
sed -i \
	-e s/com.github.ibizaman/"com.github.$githubuser"/g \
	-e s/hs-template-nix-reflexfrp/"$projectname"/g \
	-e s/hstemplatenixreflexfrp/"${projectname//-}"/g \
	"$fullpath/release.nix"
cp "shell.ghcjs.nix"      "$fullpath"
cp "shell.nix"            "$fullpath"

cp -r ".github"           "$fullpath"
sed -i \
	-e s/ibizaman/"$cachixhandle"/g \
	"$fullpath/.github/workflows/workflow.yml"

mkdirorexit "$fullpath/common"
mkdirorexit "$fullpath/backend"
mkdirorexit "$fullpath/frontend"
cp -r \
   "common/package.yaml" \
   "common/src" \
   "common/test" \
   "common/CHANGELOG.md" \
   "$fullpath/common"
cp -r \
   "backend/package.yaml" \
   "backend/app" \
   "backend/src" \
   "backend/test" \
   "backend/CHANGELOG.md" \
   "$fullpath/backend"
cp -r \
   "frontend/package.yaml" \
   "frontend/app" \
   "frontend/src" \
   "frontend/CHANGELOG.md" \
   "$fullpath/frontend"

sed -i \
	-e s/ibizaman/"$githubuser"/g \
	-e s/hs-template-nix-reflexfrp/"$projectname"/g \
	-e s/"Pierre Penninckx"/"$fullname"/g \
	-e s/ibizapeanut@gmail.com/"$fullname"/g \
	-e s/2020/"$(date +%Y)"/g \
	"$fullpath/common/package.yaml" \
	"$fullpath/backend/package.yaml" \
	"$fullpath/frontend/package.yaml"

echo "Project is created"

echo
echo "Tasks to complete:"
echo "* Please update the android.frontend and ios.frontend fields in $fullpath/release.nix"
echo "* Cachix is configured to use $cachixhandle, please make sure you have read and write access to that project."
echo "* Please create the CACHIX_${projectname^^}_AUTHTOKEN variable in github containing the auth token for cachix, otherwise the github workflow will fail."
echo "* Comment out some sections of modd.conf"
