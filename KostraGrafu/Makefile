SWIPL := swipl
SWIFLAGS := -q -g start

SRC = flp20-log.pl

all: flp20-log

flp20-log: $(SRC)
	$(SWIPL) $(SWIFLAGS) -o $@ -c $(SRC)

pack:
	zip -r flp-log-xpavel34.zip Makefile README $(SRC) tests/

.PHONY: clean
clean:
	rm -f flp20-log 2>/dev/null