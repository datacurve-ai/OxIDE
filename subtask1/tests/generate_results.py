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

    # Check for build success
    if "Finished" in logs and "target(s) in" in logs:
        results['build'] = 'passed'
    else:
        results['build'] = 'failed'

    # Check for unit tests success
    unit_tests_pattern = r'test result: .*ok.*\. (\d+) passed; (\d+) failed;'
    unit_tests_match = re.search(unit_tests_pattern, logs)
    if unit_tests_match and unit_tests_match.group(2) == '0':
        results['unit_tests'] = 'passed'
    else:
        results['unit_tests'] = 'failed'

    # Check for interaction test success
    if 'Interaction test passed' in logs:
        results['interaction_test'] = 'passed'
    else:
        results['interaction_test'] = 'failed'

    # Check for Zig syntax highlighting test
    zig_test_pattern = r'test tests::test_zig_syntax_highlighting \.\.\. .*ok'
    zig_test_match = re.search(zig_test_pattern, logs, re.IGNORECASE)
    if zig_test_match:
        results['zig_syntax_test'] = 'passed'
    else:
        results['zig_syntax_test'] = 'failed'

    return results

if __name__ == '__main__':
    results = parse_docker_output()
    with open('results.json', 'w') as f:
        json.dump(results, f, indent=4)
