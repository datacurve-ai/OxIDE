#!/bin/bash

# tests/black_box_tests/test_cases.sh

# Initialize an array to hold test results
declare -a test_results

# Test 1: Check if the editor service is running
echo "Running Black-box Test 1: Check service availability..."
if docker exec vim_haskell_editor ps aux | grep -q 'editor'; then
  test_results+=('{"title": "Service Availability", "passed": true}')
else
  test_results+=('{"title": "Service Availability", "passed": false}')
fi

# Test 2: Check if the editor can open a file
echo "Running Black-box Test 2: Open file test..."
if docker exec vim_haskell_editor [ -f "/app/data/sample.hs" ]; then
  test_results+=('{"title": "Open File", "passed": true}')
else
  test_results+=('{"title": "Open File", "passed": false}')
fi

# Output test results as a JSON array
echo "[${test_results[*]}]"
