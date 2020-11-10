# This Dockerfile constructs an environment in which Grain is built and can be used.
FROM node:14-buster
LABEL name="Grain"
LABEL description="Grain CLI and Runtime"
LABEL vcs-url="https://github.com/grain-lang/grain"
LABEL maintainer="philip@grain-lang.org"

COPY . /grain

WORKDIR /grain

RUN yarn --pure-lockfile
RUN yarn compiler build

# Set up container environment
WORKDIR /
CMD [ "/bin/bash" ]
