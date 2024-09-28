#!/bin/bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SOLUTION_DIR="$SCRIPT_DIR/../../../subtask1"
TESTS_DIR="$SCRIPT_DIR/.."
TEST_RESULTS="$TESTS_DIR/test_scripts/results.json"

cleanup() {
    echo "Cleaning up..."
    if [ -f "$SOLUTION_DIR/docker-compose.yml" ]; then
        cd "$SOLUTION_DIR"
        docker-compose down
    fi
    rm -f "$SOLUTION_DIR/Dockerfile" "$SOLUTION_DIR/docker-compose.yml"
}

trap cleanup EXIT

echo "Injecting Dockerfile and docker-compose.yml..."
cp "$TESTS_DIR/Dockerfile" "$SOLUTION_DIR/Dockerfile"
cp "$TESTS_DIR/docker-compose.yml" "$SOLUTION_DIR/docker-compose.yml"

echo "Building and starting Docker container..."
cd "$SOLUTION_DIR"
docker-compose up -d --build

echo "Waiting for container to start..."
for i in {1..30}; do
    if docker-compose ps | grep -q "Up"; then
        break
    fi
    sleep 1
done

if ! docker-compose ps | grep -q "Up"; then
    echo "Error: Container failed to start"
    exit 1
fi

echo "Running black-box tests..."
cd "$TESTS_DIR/black_box_tests"
BLACK_BOX_RESULTS=$(./test_cases.sh)
echo "$BLACK_BOX_RESULTS"
echo "Running unit tests..."
cd "$SOLUTION_DIR"
UNIT_TEST_RESULTS=$(cabal test)
echo "$UNIT_TEST_RESULTS"

# Process the results using the Python script
FINAL_JSON=$(echo "$BLACK_BOX_RESULTS" | echo "$UNIT_TEST_RESULTS" | python3 $TESTS_DIR/test_scripts/process_results.py)

echo "$FINAL_JSON" > "$TEST_RESULTS"

echo "Testing complete. Results are available in test_scripts/results.json"