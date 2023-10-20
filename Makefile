examples/%/input: .year .cookie
	mkdir -p $$(dirname $@)
	curl https://adventofcode.com/$$(cat .year)/day/$*/input -H"Cookie: $$(cat .cookie)" -sSfo $@

DAYS:=$(shell bash -c 'echo {1..25}')
PARTS:=1 2 both
# Use bash to expand this splat so I don't 
ALL_INPUTS=$(shell bash -c 'echo examples/{1..25}/input')

define PUZZLE_TGT
.PHONY:
puzzle_$(day)_$(part): examples/$(day)/input
	cabal run AdventOfCode -- puzzle $(day) $(part)

.PHONY:
test_$(day)_$(part):
	cabal run AdventOfCode -- test $(day) $(part)
endef

$(foreach day, $(DAYS), $(foreach part, $(PARTS), $(eval $(PUZZLE_TGT))))

.PHONY:
inputs: $(ALL_INPUTS)

.PHONY:
all:
	make inputs || true
	cabal run AdventOfCode -- puzzle all

.PHONY:
test-all:
	cabal run AdventOfCode -- test all

.PHONY:
lint-fix:
	./doLint.sh
