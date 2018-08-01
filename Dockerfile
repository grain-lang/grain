# TEMPORARY CONTAINER USED TO BUILD GRAIN
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

# COPY ONLY NEEDED FILES TO THIS IMAGE
WORKDIR /opt/grain
COPY --from=compiler /opt/grain/cli ./cli
COPY --from=compiler /opt/grain/runtime ./runtime
COPY --from=compiler /opt/grain/_build ./_build

# ADD COMPILER TO PATH
ENV PATH="/opt/grain/_build/install/default/bin:${PATH}"

# INSTALL RUNTIME
WORKDIR /opt/grain/runtime
RUN npm install
RUN npm run build

# INSTALL CLI
WORKDIR /opt/grain/cli
RUN npm install
RUN ln -s $PWD/bin/grain.js ../grain
ENV PATH="/opt/grain:${PATH}"

# SETUP FOR END USER
WORKDIR /root
RUN echo '3 + 3' > test.gr
RUN grainc test.gr
ENTRYPOINT ["/bin/bash"]