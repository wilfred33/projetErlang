.PHONY: all clean test

ERLC = erlc
ERL = erl

SOURCES = transaction.erl merkle_tree.erl block.erl node.erl builder.erl test_simple.erl
BEAMS = $(SOURCES:.erl=.beam)

all: $(BEAMS)

%.beam: %.erl
	$(ERLC) $<

test: all
	$(ERL) -noshell -eval "test_simple:test_builder_create_block(), init:stop()."

clean:
	del /Q *.beam blockchain_*.csv 2>nul || true

run: test
