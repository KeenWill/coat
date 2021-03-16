FROM williamgoeller/ocaml-docker:alpine

RUN mkdir /coat_deps
WORKDIR /coat_deps
COPY ./coatc.opam /coat_deps
RUN opam install . -y --deps-only --with-test
