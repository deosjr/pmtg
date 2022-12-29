.PHONY: run, test, json

run: 
	swipl -l mtg.pl -t run

test:
	swipl -l mtg.pl -t run_tests

json:
	# copied using curl from scryfall's API and dumped into 7ed.json (gitignored)
	cat 7ed.json | jq '.data[] | {name, mana_cost, type_line, power, toughness, oracle_text}'
