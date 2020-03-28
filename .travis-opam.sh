# This script is from https://github.com/ocaml/ocaml-ci-scripts

## ISC License

# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.

# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

echo -en "travis_fold:start:prepare.ci\r"
# If a fork of these scripts is specified, use that GitHub user instead
fork_user=${FORK_USER:-ocaml}

# If a branch of these scripts is specified, use that branch instead of 'master'
fork_branch=${FORK_BRANCH:-master}

### Bootstrap

set -uex

get() {
  wget https://raw.githubusercontent.com/${fork_user}/ocaml-ci-scripts/${fork_branch}/$@
}

test "$TRAVIS_REPO_SLUG" = "ocaml/ocaml-ci-scripts" || \
  get .travis-ocaml.sh
sh .travis-ocaml.sh

export OPAMYES=1
eval $(opam config env)

opam depext -y conf-m4
if [ "$TRAVIS_REPO_SLUG" = "ocaml/ocaml-ci-scripts" ] ; then
  opam pin add travis-opam --kind=path .
else
  opam pin add travis-opam https://github.com/${fork_user}/ocaml-ci-scripts.git#${fork_branch}
fi
cp ~/.opam/$(opam switch show)/bin/ci-opam ~/

opam remove -a travis-opam

mv ~/ci-opam ~/.opam/$(opam switch show)/bin/ci-opam

opam install jbuilder
opam install . --deps-only

### Setup Grain runtime and cli

cd runtime && npm i && npm run build && cd -
cd stdlib && npm i && npm run build && cd -
cd cli && npm i && npm link && cd -

echo -en "travis_fold:end:prepare.ci\r"
make
make install
make tests
