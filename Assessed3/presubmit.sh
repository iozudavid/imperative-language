#!/bin/bash

# All configuration for the presubmit is in this file. To prepare a new
# exercise, it should suffice to copy the  'presubmit-res' directory verbatim,
# and tweak the parameters in the file you are reading now. Note that in fp-
# teaching, presubmit-res is a symlink, and so to copy everything to fp-
# learning you need to use 'cp -rLt path/to/learning/assignment presubmit*'.

# To run the presubmit, we copy the student .hs files to a temporary
# directory, as well as these files.
#
# Multiple strings should be separated by a bare comma.
export PRESUBMIT_AUXILIARY_FILES_COPY=Mould.hs,AbstractSyntax.hs,IOPrime.hs,Parsing.hs,Unsolved.hs

# Some of the auxiliary files will be Haskell modules that do not import any
# student-written code, and so we try to GHC on them, that should always
# compile without problems. This is a good way to see if the student has a
# decently-working environment.
#
# Multiple strings should be separated by a bare comma.
export PRESUBMIT_AUXILIARY_PRETEST_MODS=AbstractSyntax,IOPrime,Parsing,Unsolved

# The base names of the Haskell files students have to make. After
# successfully compiling the "pretest" modules above, the presubmit will try
# to compile each of these student-supplied modules.
#
# Multiple strings should be separated by a bare comma.
export PRESUBMIT_STUDENT_MODS=Parser,PrettyPrinting,Interpreter,Optimization

# presubmit.hs will additionally check each import in these student modules
# against a whitelist, consisting of a built-in list of modules that are
# preinstalled in the lab, augmented with these modules.
#
# Multiple strings should be separated by a bare comma.
export PRESUBMIT_IMPORT_WHITELIST_CUSTOM=AbstractSyntax,IOPrime,Interpreter,Parsing,Unsolved

# After compiling the teacher-supplied "pretest" modules, and after compiling
# the student modules, the presubmit will compile Mould.hs. This is supposed
# make sure that everything in the student modules has the right type. The
# Mould.hs filename is hardcoded.

# Whether the presubmit should show commands as they are being run. This can
# make the output noisy, but it can also help students reproduce the
# presubmit's results so they better understand why it fails or succeeds.
export PRESUBMIT_VERBOSE=1

# Continue execution in presubmit.bash.

bash <<"EOD"

source presubmit-res/presubmit.bash

if [ $RESULT = 1 ]; then
    echo "Presubmit error; not creating .zip file for Canvas"
    exit $RESULT
elif [ $RESULT = 0 ]; then
    echo "Creating .zip file for submission on Canvas."
elif [ $RESULT = 2 ]; then
    echo "There were problems, but creating .zip file for submission on Canvas regardless."
    echo
else
    echo "INTERNAL ERROR: unrecognised exit code"
    exit $RESULT
fi

ZIPFILE=submission-Assessed3.zip

echo "Gathering student-produced files in $ZIPFILE for submission on Canvas."
echo

rm -f $ZIPFILE
zip $ZIPFILE Parser.hs PrettyPrinting.hs Interpreter.hs Optimization.hs

if [ $RESULT = 2 ]; then
    echo
    echo "Please note that we recommend to run the presubmit AGAIN after"
    echo "fixing the aforementioned problems. This .zip file is provided as"
    echo "a convenience even though the presubmit check failed."
fi

EOD
