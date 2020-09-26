.PHONY: run, test

run: 
	swipl -l mtg.pl -t run

test:
	swipl -l mtg.pl -t run_tests
