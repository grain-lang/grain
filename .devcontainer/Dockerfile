# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.134.0/containers/javascript-node/.devcontainer/base.Dockerfile
FROM mcr.microsoft.com/vscode/devcontainers/javascript-node:14

RUN sudo -u node npm install -g esy

# We build the dependencies with esy and export the dependencies
# much like CI for a faster startup
COPY ./compiler/esy.json /prebuild/
COPY ./compiler/esy.lock /prebuild/

WORKDIR /prebuild
RUN esy install
RUN esy build-dependencies
RUN esy export-dependencies
