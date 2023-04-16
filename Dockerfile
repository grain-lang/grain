# This Dockerfile constructs an environment in which the Grain compiler can be built and used.

FROM ospencer/esy:alpine as esy
FROM node:18

LABEL name="Grain"
LABEL description="Grain CLI"
LABEL vcs-url="https://github.com/grain-lang/grain"
LABEL maintainer="team@grain-lang.org"

COPY . /grain

WORKDIR /grain

# esy does not currently ship linux/arm64 binaries, so we manually patch it in

# Install dependencies but don't allow esy's postinstall script to run
RUN npm ci --ignore-scripts
# This line is technically incorrect on amd64, but docker does not support
# conditional copies and the arm64 folder is ignored on amd64 anyway
COPY --from=esy /app/_release /grain/node_modules/esy/platform-linux-arm64
# Manually run esy's postinstall script
RUN cd node_modules/esy && npm run postinstall

# Necessary because we disabled scripts during the original install
RUN npm run prepare

# Build the compiler and CLI
RUN npm run compiler build

# Set up container environment
WORKDIR /
CMD [ "/bin/bash" ]
