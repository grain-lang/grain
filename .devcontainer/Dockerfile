# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.134.0/containers/javascript-node/.devcontainer/base.Dockerfile
FROM mcr.microsoft.com/vscode/devcontainers/javascript-node:14

# We want to cache the heavy stuff, so we first just copy in the dependencies
# and install them with yarn/esy:
COPY ./package.json /
COPY ./compiler/package.json /compiler/
COPY ./runtime/package.json /runtime/
COPY ./cli/package.json /cli/
COPY ./stdlib/package.json /stdlib/
COPY ./compiler/esy.json /compiler
COPY ./compiler/esy.lock /compiler
COPY ./yarn.lock /

RUN yarn install --pure-lockfile
RUN yarn compiler setup
# Slow!
RUN yarn compiler build-dependencies
