import json
import re

def parse_docker_output():
    results = {
        'build': 'unknown',
        'unit_tests': 'unknown',
        'interaction_test': 'unknown',
        'zig_syntax_test': 'unknown'
    }

    # Read the Docker container logs
    with open('docker_output.log', 'r') as f:
        logs = f.read()

    # Check for build success/failure
    if 'Finished `release` profile [optimized] target(s) in' in logs:
        results['build'] = 'passed'
    else:
        results['build'] = 'failed'

    # Check for unit tests success/failure
    unit_tests_pattern = r'test result: (ok|FAILED)\. (\d+) passed; (\d+) failed;'
    unit_tests_match = re.search(unit_tests_pattern, logs)
    if unit_tests_match:
        if unit_tests_match.group(1) == 'ok' and unit_tests_match.group(3) == '0':
            results['unit_tests'] = 'passed'
        else:
            results['unit_tests'] = 'failed'

    # Check for interaction test success/failure
    if 'Interaction test passed' in logs:
        results['interaction_test'] = 'passed'
    elif 'Interaction test failed' in logs:
        results['interaction_test'] = 'failed'

    # Check for Zig syntax highlighting test
    if 'test tests::test_zig_syntax_highlighting ... ok' in logs:
        results['zig_syntax_test'] = 'passed'
    else:
        results['zig_syntax_test'] = 'failed'

    return results

if __name__ == '__main__':
    results = parse_docker_output()
    with open('results.json', 'w') as f:
        json.dump(results, f, indent=4)
