CC=gcc
LINK = $(CC)
CFLAGS = -O2 -ansi -Wall
LDFLAGS = -s

all:	miditest  helloworld

miditest:   miditest.c   midifile.o	
	$(CC) $(CFLAGS) $(LFLAGS) midifile.o miditest.c -o miditest 

helloworld: cflatfile.c midifile.o
	$(CC) $(CFLAGS) $(LFLAGS) midifile.o cflatfile.c -o helloworld

makenotetest:   makenotetest.c   midifile.o	
	$(CC) $(CFLAGS) $(LFLAGS) midifile.o makenotetest.c -o makenotetest 

#mfc120: mfcmain.c   mfc120.c   midifile.o
#	$(CC) $(CFLAGS) $(LFLAGS) midifile.o mfc120.c mfcmain.c -o mfc120

#mididump: mididump.c midiutil.o midifile.o
#	$(CC) $(CFLAGS) $(LFLAGS) midifile.o midiutil.o mididump.c -o mididump

midifile.o:	midifile.c	midifile.h
#midiutil.o:	midiutil.c	midiutil.h


install:
	@echo Just copy the files somewhere useful!

clean:
	rm -f *.o  *.mid
	rm -f miditest mozart mfc120 mididump m2rtttl

