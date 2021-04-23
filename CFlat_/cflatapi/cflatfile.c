/*The CFlat API. Codegen should call functions from this library */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#ifndef  __APPLE__
#include <malloc.h>
#endif
#include "midifile.h"


/* Note struct */
struct note{
    char tlit[3];
    int olit;
    char rlit[3];
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
void bplay_note(struct note *n, int beat); /* bplay_note takes in beat (beats/min) */

/*INPUT: Takes in a pointer to an array of note struct pointers */ 
/*OUTPUT: A midifile called "notearray.mid" that plays a C Major scale.  */ 
void play_note_arr(struct note *note_arr[]);
void bplay_note_arr(struct note *note_arr[], int beat); /* bplay_note_arr takes in beat (beats/min) */

/*INPUT: Takes in a pointer to a single note struct and pointer to a midi_file */ 
/*OUTPUT: No output but it will add the note to the midi_file */ 
void add_note(struct note *note_ptr, MIDI_FILE *mf, int track, int beat);

/*INPUT: Takes in a pointer to an array of note struct, pointer to a midi_file */ 
/*OUTPUT: No output but it will add track with notes to the midifile */ 
void add_track(struct note *note_arr[], MIDI_FILE *mf, int track, int beat);

void play_note(struct note *note_ptr) {
/*char *tlit, int olit, char *rlit */
MIDI_FILE *mf;
	if ((mf = midiFileCreate("hellonote.mid", TRUE))){
		add_note(note_ptr, mf, 1, 120);
		midiFileClose(mf);
		}
}

void bplay_note(struct note *note_ptr, int beat) {
/*char *tlit, int olit, char *rlit */
MIDI_FILE *mf;
	if ((mf = midiFileCreate("hellonotebeat.mid", TRUE))){
		add_note(note_ptr, mf, 1, beat);
		midiFileClose(mf);
		}
}

void play_note_arr(struct note *note_arr[]){

    MIDI_FILE *mf;
    if ((mf = midiFileCreate("helloarray.mid", TRUE))){

        while (*note_arr){

            printf("%s%d\n", "OCTAVE,", (*note_arr) -> olit);
            printf("%s%s\n", "RHYTHM,", (*note_arr) -> rlit);
            printf("%s%s\n", "OCTAVE,", (*note_arr) -> tlit);

            add_note((*note_arr), mf, 1, 120);
            note_arr++;
        }

        midiFileClose(mf);
    }
}

void bplay_note_arr(struct note *note_arr[], int beat){
    MIDI_FILE *mf;
    if ((mf = midiFileCreate("helloarraybeat.mid", TRUE))){
        while (*note_arr){
            add_note((*note_arr), mf, 1, beat);
            note_arr++;
        }
        midiFileClose(mf);
    }  
}

void add_note(struct note *note_ptr, MIDI_FILE *mf, int track, int beat){

    char *tlit = note_ptr -> tlit;
    int olit = note_ptr -> olit;
    char *rlit = note_ptr -> rlit;

    int miditone = 0;
    int is_rest = 0;
    char tone = tlit[0];
    printf("%s\n", tlit);
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

    /*char acc = tlit[1];*/
    char acc = tlit[1];
    if (acc == '-') {accidental = -1;} 
    else if (acc == '+') {accidental = 1;}
    else if (acc == '.') {accidental = 0;}
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

    else {printf("%s%c\n", "Rhythm is not a valid rhythm.", rhythm);}


    if (dotted) { midirhythm += midirhythm/2; }  /* adds the dotted portion */

    /*CONVERTING CFLAT olit => MIDI OCTAVE*/
    int midioctave = olit; 
    if (olit >= 0 && olit <= 10){
        midioctave *= 12;
    }
    /*DEFAULT TIME SIGNATURE */
    midiSongAddTempo(mf, track, beat);
    midiSongAddSimpleTimeSig(mf, track, 4, MIDI_NOTE_CROCHET); 

    midiFileSetTracksDefaultChannel(mf, track, MIDI_CHANNEL_1); 
    midiTrackAddProgramChange(mf, track, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);    /*We only want to set this as instrument if instrument is not set already*/
    midiTrackAddText(mf, track, textLyric, tlit);

    int volume = MIDI_VOL_MEZZO;
    if (is_rest){
        volume = 0;
    }
    midiTrackAddNote(mf, track, miditone + midioctave, midirhythm, volume, TRUE, FALSE);

}

void add_track(struct note *note_arr[], MIDI_FILE *mf, int track, int beat){

    printf("%d", track);
    while (*note_arr){
        add_note((*note_arr), mf, track, beat);
        note_arr++;
    }
}
/*struct note *A[], struct note *B[]*/
void play_tracks(int num_tracks, ){
    MIDI_FILE *mf;
    va_list valist;
    int i;
    if ((mf = midiFileCreate("playTracks.mid", TRUE))){

        
        add_track(A, mf, 1, 120);
        add_track(B, mf, 2, 120);

        midiFileClose(mf);
    }
}

/*DRIVER CODE FOR PLAY_NOTE_ARR: Creates a midifile with a C Major Scale */
int main(int argc, char* argv[])
{
    /*Need the size of the array*/
    
    int num_notes = 12;
    struct note *arr[num_notes + 1];

    /* Creates space for the array */
    int i;
    for (i=0; i<num_notes; i++){
        arr[i] = malloc(sizeof(struct note*));
    }
    
    arr[num_notes] = NULL;
    struct note *c = new_note("C", 4, "s");
    struct note *d = new_note("D", 4, "s.");
    struct note *e = new_note("E", 4, "e");
    struct note *f = new_note("F+", 4, "e.");
    struct note *g = new_note("G", 4, "q");
    struct note *a = new_note("R", 4, "q.");
    struct note *b = new_note("B", 4, "h");
    struct note *c_2 = new_note("C", 5, "h.");
    struct note *d_2 = new_note("R", 5, "w");
    struct note *e_2 = new_note("E", 5, "w");
    struct note *f_2 = new_note("F", 5, "w");
    struct note *c_flat = new_note("C-", 5, "h");

    arr[0] = c;
    arr[1] = d;
    arr[2] = e;
    arr[3] = f;
    arr[4] = g;
    arr[5] = a;
    arr[6] = b;
    arr[7] = c_2;
    arr[8] = d_2;
    arr[9] = e_2;
    arr[10] = f_2;
    arr[11] = c_flat;
/*
    arr[9] = d_2;
    arr[10] = d_s;
    arr[11] = e_2;
    arr[12] = f_2;
    arr[13] = f_s;
    arr[14] = g_2;
    arr[15] = g_s;
    arr[16] = a_2;
    arr[17] = a_s;
    arr[18] = b_2;
    arr[20] = c_3;  */


    /*bplay_note(c, 60);
    bplay_note(c, 160);

    play_note_arr(arr);
    bplay_note_arr(arr, 360);*/ 


    struct note *A[4];  A[3] = NULL;
    struct note *B[4];  B[3] = NULL;

    for (i=0; i<3; i++){
        A[i] = malloc(sizeof(struct note*));
        B[i] = malloc(sizeof(struct note*));
    }
     
    struct note *et = new_note("E", 4, "h");
    struct note *dt = new_note("D", 4, "h");
    struct note *ct = new_note("C", 4, "h");

    struct note *gt = new_note("G", 4, "h");
    struct note *ft = new_note("F", 4, "h");

    

    A[0] = et;
    A[1] = dt;
    A[2] = ct;

    B[0] = gt;
    B[1] = ft;
    B[2] = et;

    play_two_tracks(A, B);


	return 0;
}