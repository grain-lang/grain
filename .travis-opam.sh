echo -en "travis_fold:start:prepare.ci\r"

### Setup Grain runtime and cli

yarn
yarn setup

echo -en "travis_fold:end:prepare.ci\r"

yarn compiler:build
yarn compiler:test

# Don't keep full builds
npx rimraf ~/.esy/3_*/b/
