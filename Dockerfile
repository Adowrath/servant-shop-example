FROM haskell:9.2

WORKDIR /app

# Update cache
RUN stack update

# Cache build dependencies
COPY ./stack.yaml ./package.yaml /app/
RUN stack build --only-dependencies

# Build our application
COPY . /app
RUN stack build

EXPOSE 8080

CMD [ "stack", "exec", "shop-example-exe" ]
