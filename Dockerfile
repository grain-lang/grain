# This Dockerfile constructs an environment in which Grain is built and can be used.
FROM node:14-buster
LABEL name="Grain"
LABEL description="Grain CLI and Runtime"
LABEL vcs-url="https://github.com/grain-lang/grain"
LABEL maintainer="philip@grain-lang.org"

RUN apt-get update
RUN apt-get install -y cmake=3.13.4-1

# We want to cache the heavy stuff, so we first just copy in the dependencies
# and install them with yarn/esy:
COPY ./package.json /grain/
COPY ./compiler/package.json /grain/compiler/
COPY ./runtime/package.json /grain/runtime/
COPY ./cli/package.json /grain/cli/
COPY ./stdlib/package.json /grain/stdlib/
COPY ./compiler/esy.json /grain/compiler
COPY ./compiler/*.opam /grain/compiler/
COPY ./compiler/esy.lock /grain/compiler
COPY ./yarn.lock /grain/

WORKDIR /grain
RUN yarn install --pure-lockfile
RUN yarn workspace @grain/compiler esy install
# Slow!
RUN yarn workspace @grain/compiler esy build-dependencies

# Now that we've done the heavy lifting, we can pull in the rest of the files
# (probably won't be cached from this point on)

COPY . /grain
RUN yarn setup
# [TODO] Figure out why we need to keep calling esy install before running each of these commands
RUN yarn workspace @grain/compiler esy install
RUN yarn workspace @grain/compiler esy compile
RUN yarn workspace @grain/compiler esy install
RUN yarn workspace @grain/compiler esy copy-compiler

# Set up container environment
WORKDIR /
