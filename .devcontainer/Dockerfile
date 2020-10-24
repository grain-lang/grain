# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.134.0/containers/javascript-node/.devcontainer/base.Dockerfile
FROM mcr.microsoft.com/vscode/devcontainers/javascript-node:14

# We want to cache the heavy stuff, so we first just copy in the dependencies
# and install them with yarn/esy:
COPY ./package.json /root/workspace/grain/
COPY ./compiler/package.json /root/workspace/grain/compiler/
COPY ./compiler/esy.json /root/workspace/grain/compiler/
COPY ./compiler/esy.lock /root/workspace/grain/compiler/
COPY ./runtime/package.json /root/workspace/grain/runtime/
COPY ./cli/package.json /root/workspace/grain/cli/
COPY ./stdlib/package.json /root/workspace/grain/stdlib/
COPY ./yarn.lock /root/workspace/grain/

WORKDIR /root/workspace/grain
RUN yarn install --pure-lockfile
RUN yarn compiler setup
# Slow!
RUN yarn compiler build-dependencies
