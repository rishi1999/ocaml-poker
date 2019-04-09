#!/bin/bash

environment=good
OCAMLV=4.06.1
OUNITV=2.0.8

OPAM_LOCATION="$(command -v opam)"
if [[ $OPAM_LOCATION == "" ]]; then
  echo "OPAM is NOT available.  This is bad."
  environment=bad
else
  echo "OPAM is available.  Good."
fi

SWITCH="$(opam switch show 2>&1)"
if [[ $SWITCH == $OCAMLV ]]; then
  echo "OPAM switch $OCAMLV is active.  Good."
else
  echo "OPAM switch $OCAMLV is NOT active.  This is bad."
  echo "The active switch is: $SWITCH"
  environment=bad
fi

OCAMLC_VERSION="$(ocamlc --version 2>&1)"
if [[ $OCAMLC_VERSION == $OCAMLV ]]; then
  echo "OCaml compiler version $OCAMLV is active.  Good."
else
  echo "OCaml compiler version $OCAMLV is NOT active.  This is bad."
  echo "The active version is: $OCAMLC_VERSION"
  environment=bad
fi

OUNIT_VERSION="$(opam info ounit -f version 2>&1)"
if [[ $OUNIT_VERSION =~ $OUNITV ]]; then
  echo "OUnit version $OUNITV is active.  Good."
else
  echo "OUnit version $OUNITV is NOT active.  This is bad."
  echo "The active version of OUnit is: $OUNIT_VERSION"
  environment=bad
fi

if [[ $environment == good ]]; then
  cat <<EOF
===========================================================
Your OCaml environment looks good to me.  Congratulations!
===========================================================
EOF
else
  cat <<EOF
===========================================================
WARNING

Your OCaml environment looks broken to me.  The code that
you submit might not compile on the grader's machine,
leading to heavy penalties.  Please fix your OCaml
environment. Check the error messages above carefully to
determine what is wrong with your environment.  See a
consultant for help if you cannot determine what is wrong.
===========================================================
EOF
fi
