/*The CFlat API. Codegen should call functions from this library */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef  __APPLE__
#include <malloc.h>
#endif
#include "midifile.h"

/* Note struct */
struct note{
    char tlit[2];
    int olit;
    char rlit[2];
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

/*Function declarations*/

/*INPUT: Takes in a pointer to a single note struct */ 
/*OUTPUT: A midifile called "hellonote.mid" that plays the note */ 
void play_note(struct note *n);

/*INPUT: Takes in a pointer to a single note struct and pointer to a midi_file */ 
/*OUTPUT: No output but it will add the note to the midi_file */ 
void add_note(struct note *note_ptr, MIDI_FILE *mf);

/*INPUT: Takes in a pointer to an array of note struct pointers */ 
/*OUTPUT: A midifile called "notearray.mid" that plays a C Major scale.  */ 
void play_note_arr(struct note *arr[]);



void play_note_arr(struct note *note_arr[]){

    MIDI_FILE *mf;
    if ((mf = midiFileCreate("notearray.mid", TRUE))){
/*
        while (*note_arr){

            printf("%s%d\n", "OCTAVE,", (*note_arr) -> olit);
            printf("%s%s\n", "RHYTHM,", (*note_arr) -> rlit);
            printf("%s%s\n", "OCTAVE,", (*note_arr) -> tlit);

            add_note((*note_arr), mf);
            note_arr++;
        }*/

        /*Currently hardcoded 8 but we will want arrays of any lengths */

        int i;
        
        /*size_t n = sizeof(note_arr)/sizeof(*note_arr);*/

        for (i=0; i<9; i++){
            add_note((note_arr[i]), mf);
        }
        midiFileClose(mf);
    }
}

void play_note(struct note *note_ptr) {
/*char *tlit, int olit, char *rlit */
MIDI_FILE *mf;
	if ((mf = midiFileCreate("hellonote.mid", TRUE))){
		add_note(note_ptr, mf);
		midiFileClose(mf);
		}
}

void add_note(struct note *note_ptr, MIDI_FILE *mf){

    char *tlit = note_ptr -> tlit;
    int olit = note_ptr -> olit;
    char *rlit = note_ptr -> rlit;

    int miditone;
    char tone = tlit[0];
    printf("%s", tlit);
    if (tone == 'C') {miditone = 0;}
    if (tone == 'D') {miditone = 2;}
    if (tone == 'E') {miditone = 4;}
    if (tone == 'F') {miditone = 5;}
    if (tone == 'G') {miditone = 7;}
    if (tone == 'A') {miditone = 9;}
    if (tone == 'B') {miditone = 11;}

    int accidental = 0;

    /*char acc = tlit[1];*/
    if (strlen(tlit) > 1 && tlit[1] == '.') {accidental = 1;}

/*
    if (acc == '-') {accidental = -1;} 
    else if (acc == '+') {accidental = 1;}
    else if (acc == '.') {accidental = 0;}*/
    /*  else {printf("%s\n", "This is not an allowable accidental value.");}*/

    miditone = (miditone + accidental)%12;    /*Accounts for any wraparound needed for B# or Cflat*/
    

    /*CONVERTING CFLAT RHYTHM => MIDI RHYTHM*/
    int midirhythm = MIDI_NOTE_CROCHET;
    int dotted = 0;
    if (strlen(rlit) > 1 && rlit[1] == '.') {dotted = 1;}   /*checks for dotted value*/

    char rhythm = rlit[0];
    if (rhythm == 's') {midirhythm = MIDI_NOTE_SEMIQUAVER;}
    else if (rhythm == 'e') {midirhythm = MIDI_NOTE_QUAVER;}
    else if (rhythm == 'q') {midirhythm = MIDI_NOTE_CROCHET;}
    else if (rhythm == 'h') {midirhythm = MIDI_NOTE_MINIM;}
    else if (rhythm == 'w') {midirhythm = MIDI_NOTE_BREVE;}

    else {printf("%s%c\n", "Rhythm: ", rhythm);}


    if (dotted) { midirhythm += midirhythm/2; }  /* adds the dotted portion */

    /*CONVERTING CFLAT olit => MIDI OCTAVE*/
    int midioctave = olit; 
    if (olit >= 0 && olit <= 10){
        midioctave *= 12;
    }
    /*DEFAULT TIME SIGNATURE */
    midiSongAddTempo(mf, 1, 120);
    midiSongAddSimpleTimeSig(mf, 1, 4, MIDI_NOTE_CROCHET); 

    midiFileSetTracksDefaultChannel(mf, 1, MIDI_CHANNEL_1);
    midiTrackAddProgramChange(mf, 1, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);    /*We only want to set this as instrument if instrument is not set already*/
    midiTrackAddText(mf, 1, textLyric, tlit);

    midiTrackAddNote(mf, 1, miditone + midioctave, midirhythm, MIDI_VOL_MEZZO, TRUE, FALSE);

}


/*DRIVER CODE FOR PLAY_NOTE_ARR: Creates a midifile with a C Major Scale */
int main(int argc, char* argv[])
{
    struct note *arr[10];
    int i;
    
    for (i=0; i<9; i++){
        arr[i] = malloc(sizeof(struct note*));
    }
    arr[9] = NULL;

    struct note *c = new_note("C", 4, "s");
    struct note *d = new_note("D", 4, "s.");
    struct note *e = new_note("E", 4, "e");
    struct note *f = new_note("F", 4, "e.");
    struct note *g = new_note("G", 4, "q");
    struct note *a = new_note("A", 4, "q.");
    struct note *b = new_note("B", 4, "h");
    struct note *c_2 = new_note("C", 5, "h.");
    struct note *d_2 = new_note("D", 5, "w");

    

    /*
    struct note *d_2 = new_note("D", 5, "q");
    struct note *d_s = new_note("D+", 5, "q");
    struct note *e_2 = new_note("E", 5, "q");
    struct note *f_2 = new_note("F", 5, "q");
    struct note *f_s = new_note("F+", 5, "q");
    struct note *g_2 = new_note("G", 5, "q");
    struct note *g_s = new_note("G+", 5, "q");
    struct note *a_2 = new_note("A", 5, "q");
    struct note *a_s = new_note("A+", 5, "q");
    struct note *b_2 = new_note("B", 5, "q");
    struct note *c_3 = new_note("C", 6, "q"); */

    arr[0] = c;
    arr[1] = d;
    arr[2] = e;
    arr[3] = f;
    arr[4] = g;
    arr[5] = a;
    arr[6] = b;
    arr[7] = c_2;
    arr[8] = d_2;


    // arr[9] = d_2;
    // arr[10] = d_s;
    // arr[11] = e_2;
    // arr[12] = f_2;
    // arr[13] = f_s;
    // arr[14] = g_2;
    // arr[15] = g_s;
    // arr[16] = a_2;
    // arr[17] = a_s;
    // arr[18] = b_2;
    // arr[20] = c_3;  

    play_note(c);
    play_note_arr(arr);
	return 0;
}