#!/usr/bin/env bash

if [ "$#" -ne 2 ]; then
    >&2 echo "Usage: $0 PKGS OPAM_NAMES"
    >&2 echo "PKGS is a comma-separated list of package names,"
    >&2 echo "as would be sent to 'ocamlbuild'. OPAM_NAMES is a"
    >&2 echo "list of corresponding OPAM package names."
    exit 1
fi

PKGS=( `echo "$1" | tr ',' ' '` )
OPAM_PKGS=( `echo "$2" | tr ',' ' '` )
CACHE="$(dirname "$0")/.installed-pkgs"

check-cache() {
    if [ ! -e "$CACHE" ]; then
        return 1
    fi
    if grep -Ff<(echo "$1") "$CACHE" &> /dev/null; then
        return 0
    fi
    return 1
}

TO_INSTALL=""

for ((i=0; i<${#PKGS[@]}; ++i)); do
    pkg="${PKGS[i]}"
    opam_pkg="${OPAM_PKGS[i]}"
    if ! check-cache "$pkg"; then
        if ! ocamlfind query "$pkg" &> /dev/null; then
            TO_INSTALL="$TO_INSTALL $opam_pkg"
        fi
        echo "$pkg" >> "$CACHE"
    fi
done

if [ ! -z "$TO_INSTALL" ]; then
    echo "Installing required package(s): $TO_INSTALL"
    opam install $TO_INSTALL || (echo "(use 'opam pin' to make sure your OCaml version is correct)" && rm "$CACHE" && exit 1)
fi
