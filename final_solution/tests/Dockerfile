# Dockerfile

FROM rust:latest

# Install necessary packages
RUN apt-get update && apt-get install -y \
    expect \
    python3 \
    pkg-config \
    lua5.4 \
    liblua5.4-dev

WORKDIR /app

COPY . /app

# Build the Rust project
RUN cargo build --release

# Expose port for Markdown preview
EXPOSE 3030

# Run the tests and output specific messages for parsing
CMD bash -c "\
    echo 'Running unit tests...'; \
    if cargo test; then echo 'Unit tests passed'; else echo 'Unit tests failed'; fi; \
    echo 'Running interaction test...'; \
    if expect tests/test_interaction.exp; then echo 'Interaction test passed'; else echo 'Interaction test failed'; fi \
"
