all: test_switch

test_switch: test_switch.c
	gcc -g -lssl -lcrypto -o test_switch test_switch.c js0n.c

clean:
	rm -f test_switch
