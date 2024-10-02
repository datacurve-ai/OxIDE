import json
import re

def parse_docker_log(file_path):
    results = {
        'unit_tests': [],
        'interaction_tests': ''
    }

    with open(file_path, 'r') as f:
        content = f.read()

    # Parse unit test results
    unit_test_cases = re.findall(r"test (\S+) ... \x1b\[32mok\x1b\(B\x1b\[m", content)
    for test_case in unit_test_cases:
        results['unit_tests'].append({
            'test': test_case,
            'status': 'ok'
        })

    # Parse interaction test result
    interaction_test_match = re.search(r"Interaction test (failed|passed)", content)
    if interaction_test_match:
        results['interaction_tests'] = interaction_test_match.group(1)

    return results

if __name__ == "__main__":
    docker_log = 'docker_output.log'
    parsed_results = parse_docker_log(docker_log)

    # Write results to JSON
    with open('results.json', 'w') as outfile:
        json.dump(parsed_results, outfile, indent=4)

    print("Docker log results written to docker_results.json")
