#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#ifndef  __APPLE__
#include <malloc.h>
#endif

/* Note struct */
struct note{
    char* tlit;
    int olit;
    char* rlit;
}note;

/*Mallocs space for and initializes a new note struct */
struct note *new_note(char *tone, int octave, char *rhythm){

    struct note *n = malloc(sizeof(struct note));
    if (n == NULL) return NULL;

    strcpy(n->tlit, tone);
    n->olit = octave;
    strcpy(n->rlit, rhythm);

    return n;
}

struct note change_tone(struct note *note, int incr, int is_lower){
    char *tlit = note -> tlit;
    int olit = note -> olit;

    char *newtlit;
    int newolit;
    char *samerlit = note -> rlit;
    struct note* newnoteptr;
    struct note newnote;

    int miditone = 0;
    int is_rest = 0;
    char tone = tlit[0];

    if (tone == 'R') {is_rest = 1;}
    else if (tone == 'C') {miditone = 0;}
    else if (tone == 'D') {miditone = 2;}
    else if (tone == 'E') {miditone = 4;}
    else if (tone == 'F') {miditone = 5;}
    else if (tone == 'G') {miditone = 7;}
    else if (tone == 'A') {miditone = 9;}
    else if (tone == 'B') {miditone = 11;}
    else {printf("%s", "This is not a valid tone.");}

    int accidental = 0;
    char acc = tlit[1];

    if (acc == '-') {accidental = -1;} 
    else if (acc == '+') {accidental = 1;}
    else if (acc == '.') {accidental = 0;}

    /*  else {printf("%s\n", "This is not an allowable accidental value.");}*/
    if(is_lower){miditone = miditone + accidental - incr;}
    else{miditone = miditone + accidental + incr;}
    
    if (is_lower){
        while(miditone < 0){
            miditone += 12;
            olit -=1;
        }
    }else{
        while (miditone > 11){
            miditone -= 12;
            olit +=1;
        }
    }

    char *toneMap[] = {"C", "C+", "D", "D+", "E", "F", "F+", "G", "G+", "A", "A+", "B"};

    strcmp(newtlit, toneMap[miditone]);
    newolit = olit;
    newnoteptr = new_note(newtlit, newolit, samerlit);
    newnote = *newnoteptr;

    return newnote;
}

int main(int argc, char* argv[]){

	struct note et;
	struct note aa;
	et.tlit = "E";
	et.olit = 4;
	et.rlit = "h";

   	aa = *(change_tone(&et, 1, 0));
	return 0;

}
