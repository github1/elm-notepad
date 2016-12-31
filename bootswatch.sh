#!/usr/bin/env bash

BASEDIR=$(dirname $0)

if [ -z $1 ]; then
  echo "no theme specified"
  exit 1
fi

mkdir -p ${BASEDIR}/src/style/theme/$1
if [ ! -f  ${BASEDIR}/src/style/theme/$1/bootswatch.less ]; then
    curl https://bootswatch.com/${1}/bootswatch.less > ${BASEDIR}/src/style/theme/$1/bootswatch.less
    curl https://bootswatch.com/${1}/variables.less > ${BASEDIR}/src/style/theme/$1/variables.less
fi
echo "@import 'theme/$1/bootswatch.less';" > ${BASEDIR}/src/style/theme.less;
echo "@import 'theme/$1/variables.less';" >> ${BASEDIR}/src/style/theme.less;


