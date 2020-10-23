# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.134.0/containers/javascript-node/.devcontainer/base.Dockerfile
FROM mcr.microsoft.com/vscode/devcontainers/javascript-node:14

RUN yarn install --pure-lockfile
RUN yarn compiler setup
# Slow!
RUN yarn compiler build-dependencies
