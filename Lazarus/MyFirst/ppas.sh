#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling synsock
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/synsock.o  -x assembler /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/synsock.s
if [ $? != 0 ]; then DoExitAsm synsock; fi
rm /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/synsock.s
echo Assembling synacode
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/synacode.o  -x assembler /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/synacode.s
if [ $? != 0 ]; then DoExitAsm synacode; fi
rm /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/synacode.s
echo Assembling project1
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target x86_64-apple-macosx10.8.0 -o /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/project1.o  -x assembler /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/project1.s
if [ $? != 0 ]; then DoExitAsm project1; fi
rm /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/lib/x86_64-darwin/project1.s
echo Linking /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/project1
OFS=$IFS
IFS="
"
/Library/Developer/CommandLineTools/usr/bin/ld     -framework Cocoa      -order_file /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/symbol_order.fpc -multiply_defined suppress -L. -o /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/project1 `cat /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/link14374.res` -filelist /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/linkfiles14374.res
if [ $? != 0 ]; then DoExitLink /Users/JimKueneman/Documents/LccOpenPascal/Lazarus/MyFirst/project1; fi
IFS=$OFS
