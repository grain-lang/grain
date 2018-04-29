#!/usr/bin/env bash

SCRIPT_DIR=$(dirname $0)
GRAIN_DIR="${SCRIPT_DIR}/.."

is_installed() {
    which "$1" > /dev/null
}

check_installed() {
    if is_installed "$1"; then
        return 0
    else
        echo >&2 "'$1' must be installed before building."
        exit 1
    fi
}

if [ -z "$GRAIN_FORCE_INSTALL" ] && is_installed jbuilder; then
    JBUILDER_DEPS="$(cd ${GRAIN_DIR} && jbuilder external-lib-deps @install --missing)"

    if [ -z "${JBUILDER_DEPS}" ]; then
        # Everything is installed
        exit 0
    else
        echo "${JBUILDER_DEPS}"
        echo ""
        echo "Installing missing dependencies..."
    fi
else
    echo "Installing dependencies..."
fi

check_installed opam

if (opam --version | grep -Eq '^2'); then
    # OPAM 2.0+
    opam install --deps-only "${GRAIN_DIR}"
else
    cd ${GRAIN_DIR} && opam install grain --deps-only
fi
