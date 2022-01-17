# This Dockerfile constructs a minimal environment in which Grain programs can be compiled.
# The environment is only meant to build Grain programs, not develop the compiler.

FROM node:16-slim

LABEL name="Grain"
LABEL description="Grain CLI"
LABEL vcs-url="https://github.com/grain-lang/grain"
LABEL maintainer="team@grain-lang.org"

COPY . /grain

WORKDIR /grain

# Build the compiler and CLI
RUN apt-get update && \
    # Install necessary build tools to build the Grain compiler
    apt-get install git curl zlib1g build-essential -y && \
    # Install all JavaScript dependencies
    yarn --pure-lockfile && \
    # Build the Grain compiler
    yarn compiler build && \
    # Remove all node modules and only install modules necessary to run the CLI (no dev dependencies)
    yarn cache clean && \
    rm -rf node_modules && \
    NODE_ENV=production yarn --pure-lockfile && \
    # Remove all esy build files
    rm -rf compiler/_esy && \
    rm -rf ~/.esy && \
    # Remove all of the build tooling and apt lists
    apt-get remove build-essential -y && \
    rm -rf /var/lib/apt/lists/*

# Set up container environment
WORKDIR /
CMD [ "/bin/bash" ]
