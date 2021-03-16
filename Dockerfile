FROM williamgoeller/coat:deps-only

RUN mkdir /coat
WORKDIR /coat
COPY . /coat

CMD ["opam", "exec", "dune", "build", "@install", "@runtest"]
