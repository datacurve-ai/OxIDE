#!/bin/bash

# tests/test_scripts/run_tests.sh

set -e


# Paths
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SOLUTION_DIR="$SCRIPT_DIR/../../solution"
TESTS_DIR="$SCRIPT_DIR/.."

TEST_RESULTS="$TESTS_DIR/test_scripts/results.json"
echo "Current working directory: $(pwd)"
echo "SOLUTION_DIR: $SOLUTION_DIR"
echo "TESTS_DIR: $TESTS_DIR"
if [ ! -f "$TESTS_DIR/Dockerfile" ]; then
    echo "Error: Dockerfile not found in tests directory"
    exit 1
fi

# Step 1: Inject Dockerfile and docker-compose.yml into the solution folder
echo "Injecting Dockerfile and docker-compose.yml into the solution folder..."
cp "$TESTS_DIR/Dockerfile" "$SOLUTION_DIR/Dockerfile"
cp "$TESTS_DIR/docker-compose.yml" "$SOLUTION_DIR/docker-compose.yml"

# Step 2: Build the Docker container
echo "Building the Docker container..."
cd "$SOLUTION_DIR"
docker-compose build

# Step 3: Start the Docker container
echo "Starting the Docker container..."
docker-compose up -d

# Wait for the container to be fully up
sleep 5

# Step 4: Run black-box tests against the running Docker container
echo "Running black-box tests..."
cd "$TESTS_DIR/black_box_tests"
BLACK_BOX_RESULTS=$(./test_cases.sh)

# Step 5: Run unit tests against the original solution code
echo "Running unit tests..."
cd "$SOLUTION_DIR"
UNIT_TEST_RESULTS=$(cabal test --show-details=direct)

# Step 6: Collect test results
echo "Collecting test results..."
cd "$TESTS_DIR/test_scripts"

# Parse results and output as JSON
echo "{" > $TEST_RESULTS
echo "  \"black_box_tests\": $BLACK_BOX_RESULTS," >> $TEST_RESULTS
echo "  \"unit_tests\": \"$UNIT_TEST_RESULTS\"" >> $TEST_RESULTS
echo "}" >> $TEST_RESULTS

# Step 7: Spin down the Docker containers
echo "Stopping the Docker container..."
cd "$SOLUTION_DIR"
docker-compose down

# Step 8: Delete the injected Dockerfile and docker-compose.yml
echo "Cleaning up..."
rm "$SOLUTION_DIR/Dockerfile"
rm "$SOLUTION_DIR/docker-compose.yml"

echo "Testing complete. Results are available in test_scripts/results.json"
