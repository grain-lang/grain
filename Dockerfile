# This Dockerfile constructs an environment in which the Grain compiler can be built and used.

FROM ospencer/esy:0.9.2 AS esy
FROM node:22

LABEL name="Grain"
LABEL description="Grain CLI"
LABEL vcs-url="https://github.com/grain-lang/grain"
LABEL maintainer="team@grain-lang.org"

COPY . /grain

WORKDIR /grain

# esy does not currently ship linux/arm64 binaries, so we manually patch it in

# Install dependencies but don't allow esy's postinstall script to run
RUN npm ci --ignore-scripts
COPY --from=esy /usr/local /grain/node_modules/esy

# Necessary because we disabled scripts during the original install
RUN npm run prepare

# Build the compiler and CLI
RUN npm run compiler build

# Set up container environment
WORKDIR /
CMD [ "/bin/bash" ]
