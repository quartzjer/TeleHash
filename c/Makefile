all: test_switch

DOTC = js0n.c jw.c line.c telex.c sha1.c
DOTH = jw.h telex.h line.h sha1.h

test_switch: test_switch.c $(DOTC) $(DOTH)
	gcc -g -lssl -lcrypto -o test_switch test_switch.c $(DOTC)

clean:
	rm -f test_switch
