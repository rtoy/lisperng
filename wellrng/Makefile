# Simple makefile to build the test programs

all : test-well1024a test-well19937a test-well44497a test-well512a

test-well1024a : test-well1024a.o well1024a.o
	$(CC) -o $@ $^

test-well19937a : test-well19937a.o well19937a.o
	$(CC) -o $@ $^

test-well44497a : test-well44497a.o well44497a.o
	$(CC) -o $@ $^

test-well512a : test-well512a.o well512a.o
	$(CC) -o $@ $^
