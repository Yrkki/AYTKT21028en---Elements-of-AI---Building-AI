#!/bin/bash

scripts=../../cv-generator/cv-generator-life-sugar/scripts
scriptName='launch'
scriptDescription='Launch job'
. "$scripts"/start.sh

home="$(echo $HOME)"
. "$scripts"/detail.sh "Home: "$'\033[0;35m'"""$home"""$'\033[1;30m'
scraperScript="$home""/source/repos/R/AYTKT21028en---Elements-of-AI---Building-AI/Exoplanets.r"
. "$scripts"/detail.sh "R script: "$'\033[0;35m'"""$scraperScript"""$'\033[1;30m'
echo

rVersion=$(reg query "HKLM\\Software\\R-core\\R" -v "Current Version" | grep -oE '[0-9]\.[0-9]\.[0-9]$')
. "$scripts"/detail.sh "R version: "$'\033[0;35m'"""$rVersion"""$'\033[1;30m'
rVersionMajor="${rVersion:0:3}"
. "$scripts"/detail.sh "R version major: "$'\033[0;35m'"""$rVersionMajor"""$'\033[1;30m'
echo

appDataLocal="$(echo $LOCALAPPDATA)"
. "$scripts"/detail.sh "App data local: "$'\033[0;35m'"""$appDataLocal"""$'\033[1;30m'
appDataLocalRLibsPath="$appDataLocal""/R/win-library/""$rVersionMajor"
. "$scripts"/detail.sh "App data local R libs: "$'\033[0;35m'"""$appDataLocalRLibsPath"""$'\033[1;30m'
echo

programFiles="$(echo $PROGRAMFILES)"
. "$scripts"/detail.sh "Program files: "$'\033[0;35m'"""$programFiles"""$'\033[1;30m'
rInstallPath="$programFiles""/R/R-""$rVersion"
. "$scripts"/detail.sh "R install path: "$'\033[0;35m'"""$rInstallPath"""$'\033[1;30m'
rInstallLibsPath="$rInstallPath""/library"
. "$scripts"/detail.sh "R install libs path: "$'\033[0;35m'"""$rInstallLibsPath"""$'\033[1;30m'
rInstallExecutablePath="$rInstallPath""/bin/x64"
. "$scripts"/detail.sh "R install executable path: "$'\033[0;35m'"""$rInstallExecutablePath"""$'\033[1;30m'
echo

export R_LIBS="\"""$appDataLocalRLibsPath""\"":"\"""$rInstallLibsPath""\""
env | grep R_LIBS
echo

r="$rInstallExecutablePath""/R.exe"
. "$scripts"/detail.sh "R path: "$'\033[0;35m'"""$r"""$'\033[1;30m'

"$r" --version

rScript="$rInstallExecutablePath""/Rscript.exe"
. "$scripts"/detail.sh "R script: "$'\033[0;35m'"""$rScript"""$'\033[1;30m'

"$rScript" --version
echo

"$rScript" "$scraperScript"
echo

. "$scripts"/finish.sh
