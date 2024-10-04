#!/bin/bash

# run_tests.sh

# Exit immediately if a command exits with a non-zero status
set -e

# Define variables for file paths
DOCKERFILE="Dockerfile"
DOCKER_COMPOSE_FILE="docker-compose.yml"
PARENT_DIR="../"
TESTS_DIR="tests"
LOG_FILE="${TESTS_DIR}/docker_output.log"

echo "Copying necessary files to parent directory..."
cp "${DOCKERFILE}" "${PARENT_DIR}"
cp "${DOCKER_COMPOSE_FILE}" "${PARENT_DIR}"
cp "Cargo.toml" "${PARENT_DIR}"
cd ..
mkdir templates
mkdir static
cd tests
cp "styles.css" "../static"
cp "preview.html" "../templates"

echo "Changing to parent directory: ${PARENT_DIR}"
cd "${PARENT_DIR}"

echo "Building and running docker compose..."
docker compose up --build --abort-on-container-exit | tee "${LOG_FILE}"

echo "Changing back to tests directory: ${TESTS_DIR}"
cd "${TESTS_DIR}"

echo "Generating results.json..."
python3 generate_results.py

echo "Cleaning up: removing Dockerfile and docker compose.yml from parent directory..."
rm "${PARENT_DIR}${DOCKERFILE}"
rm "${PARENT_DIR}${DOCKER_COMPOSE_FILE}"

echo "Test run complete."
