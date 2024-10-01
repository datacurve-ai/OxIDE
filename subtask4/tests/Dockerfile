FROM rust:latest

RUN apt-get update && apt-get install -y expect python3

WORKDIR /app

COPY . /app

RUN cargo build --release

# Run the tests and output specific messages for parsing
CMD bash -c " \
    echo 'Running unit tests...'; \
    if cargo test; then echo 'Unit tests passed'; else echo 'Unit tests failed'; fi; \
    echo 'Running interaction test...'; \
    if expect tests/test_interaction.exp; then echo 'Interaction test passed'; else echo 'Interaction test failed'; fi \
"
