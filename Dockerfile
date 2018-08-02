# TEMPORARY IMAGE USED TO BUILD GRAIN
FROM ocaml/opam2:4.05 as compiler

# INSTALL SYSTEM DEPENDECIES
USER root
RUN apt-get install m4 -y

# COPY REPOSITORY INTO IMAGE
WORKDIR /opt/grain
COPY . .
RUN chown -R opam .
USER opam

# INSTALL OPAM DEPENDECIES
RUN eval $(opam env) && opam install . --deps-only -y

# BUILD GRAIN COMPILER DEPENDECIES
RUN eval $(opam env) && make

# SWITCH TO NODE IMAGE
FROM node:10.7

# ADD EDITOR FOR CONVENIENCE
RUN apt-get update && \
    apt-get install -y vim && \
    rm -rf /var/lib/apt/lists/*

# COPY ONLY NEEDED FILES TO THIS IMAGE
WORKDIR /opt/grain
COPY --from=compiler /opt/grain/_build ./_build
COPY ./cli ./cli
COPY ./runtime ./runtime

# ADD COMPILER TO PATH
ENV PATH="/opt/grain/_build/install/default/bin:${PATH}"

# INSTALL RUNTIME
WORKDIR /opt/grain/runtime
RUN npm install && \
    npm run build

# INSTALL CLI
WORKDIR /opt/grain/cli
RUN npm install && \
    npm link

# SETUP FOR END USER
WORKDIR /root
CMD ["bash"]