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

echo "Running unit tests..."
cd "$SOLUTION_DIR"
UNIT_TEST_RESULTS=$(cabal test)

FINAL_JSON=$(python -c "
import json
import re

black_box_results = '''$BLACK_BOX_RESULTS'''
unit_test_results = '''$UNIT_TEST_RESULTS'''

# Clean up black box results
clean_black_box_results = re.search(r'\[.*\]', black_box_results, re.DOTALL)
if clean_black_box_results:
    clean_black_box_results = json.loads(clean_black_box_results.group())
else:
    clean_black_box_results = []

output = {
    'black_box_tests': clean_black_box_results,
    'unit_tests': unit_test_results.strip()
}

print(json.dumps(output, indent=2))
")

echo "$FINAL_JSON" > "$TEST_RESULTS"

echo "Testing complete. Results are available in test_scripts/results.json"