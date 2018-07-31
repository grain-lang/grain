#!/bin/sh

# Packages required that must be
# installed with 'apt-get'.
REQ_PACKS=('ocaml' 'opam' 'jbuilder')

# Packages required that must be
# installed with 'opam'.
REQ_OPAM_PACKS=('batteries'
                'cmdliner'
                'dypgen'
                'extlib'
                'oUnit'
                'ocaml-migrate-parsetree'
                'ocamlgraph'
                'ppx_deriving'
                'ppx_sexp_conv'
                'sexplib'
                'stdint'
                'wasm')

# Inform the user of what changes might be
# applied during the installation.
echo "The following operations are"
echo "required to install 'grain':"

# List packages that might be installed.
for PACK in ${REQ_PACKS[@]}; do
    echo "    * $PACK might be installed with 'apt-get'"
done

# Prompting for consent.
echo -n "Are you ok with this[y/n]: "
read ANS

# Checking wether the user is fine with
# those changes.
if [ "$ANS" = "n" ]; then
    echo "Alright, you don't want"
    echo "us to do that so were aborting!"
    exit 1
fi

# Installing the required packages.
echo ' ~~~ BEGIN APT-GET OUTPUT ~~~'
sudo apt-get install ${REQ_PACKS[@]}
echo -n ' ~~~ END APT-GET OUTPUT ~~~'

# Either continue the installation
# or abort due to errors whilst
# calling 'apt-get'.
if [ $? ]; then
    # Installing 'opam' required packages.
    opam init                         &&
    opam install ${REQ_OPAM_PACKS[@]} &&

    # Building and installing 'grain'.
    make                              &&
    make install                      &&
    make tests
else
    # Notify the user of the failure.
    echo "Couldn't continue installation,"
    echo "so I'm aborting it"
fi
