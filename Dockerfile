# This Dockerfile constructs an environment in which the Grain compiler can be built and used.

FROM node:16

LABEL name="Grain"
LABEL description="Grain CLI"
LABEL vcs-url="https://github.com/grain-lang/grain"
LABEL maintainer="team@grain-lang.org"

COPY . /grain

WORKDIR /grain

# Build the compiler and CLI
RUN yarn --pure-lockfile && \
    yarn compiler build

# Set up container environment
WORKDIR /
CMD [ "/bin/bash" ]
