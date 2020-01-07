import json
import sys
import random

if __name__ == "__main__":
    result = json.load(sys.stdin)

    # Terminate if unsat
    if result['Result'] == 'UNSATISFIABLE':
        print(result['Result'])
        sys.exit(1)

    calls = result['Calls']
    witnesses = result['Call'][random.randrange(calls)]['Witnesses']
    num = result['Models']['Number']
    # Select random witness and its facts
    facts = witnesses[random.randrange(num)]['Value']

    # Transform list of facts into answer set program
    print("\n".join(map(lambda fact: f'{fact}.', facts)))
