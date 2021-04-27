/*The CFlat API. Codegen should call functions from this library */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#ifndef  __APPLE__
#include <malloc.h>
#endif
#include "midifile.h"
#include "cflatapi.h"

/*Mallocs space for and initializes a new note struct */
struct note *new_note(char *tone, int octave, char *rhythm){

    struct note *n = malloc(sizeof(struct note));
    if (n == NULL) return NULL;

    strcpy(n->tlit, tone);
    n->olit = octave;
    strcpy(n->rlit, rhythm);

    return n;
}


/* INTERNAL FUNCTIONS, CFLAT USERS SHOULD NOT CALL THESE */

/*INPUT: Takes in a pointer to a single note struct and pointer to a midi_file */ 
/*OUTPUT: No output but it will add the note to the midi_file */ 
void add_note(struct note *note_ptr, MIDI_FILE *mf, int track);

/*INPUT: Takes in a pointer to an array of note struct, pointer to a midi_file */ 
/*OUTPUT: No output but it will add track with notes to the midifile */ 
void add_track(struct note *note_arr[], MIDI_FILE *mf, int track);


void play_note(struct note *note_ptr, char *filename) {
    MIDI_FILE *mf;
    char name[100];
    strcpy(name, filename);
    char midi[] = ".mid";
    strcat(name, midi);
	if ((mf = midiFileCreate(name, TRUE))){
        midiTrackAddProgramChange(mf, 1, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);   
		add_note(note_ptr, mf, 1);
		midiFileClose(mf);
	}
}

void bplay_note(struct note *note_ptr, int beat, char *filename) {
MIDI_FILE *mf;
	if ((mf = midiFileCreate("output.mid", TRUE))){
        midiSongAddTempo(mf, 1, beat);
        midiTrackAddProgramChange(mf, 1, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);   
		add_note(note_ptr, mf, 1);
		midiFileClose(mf);
		}
}

void iplay_note(struct note *note_ptr, int instrument) {
MIDI_FILE *mf;
	if ((mf = midiFileCreate("hellonoteinst.mid", TRUE))){

        midiTrackAddProgramChange(mf, 1, instrument);   
		add_note(note_ptr, mf, 1);
		midiFileClose(mf);
		}
}

void play_note_arr(struct note *note_arr, char *filename){

    MIDI_FILE *mf;
    char name[100];
    strcpy(name, filename);
    char midi[] = ".mid";
    strcat(name, midi);
    if ((mf = midiFileCreate(name, TRUE))){
        int i = 0;
        for (i = 0; i < 23; i++) {
/*
            printf("%s%d\n", "OCTAVE,", (*note_arr) -> olit);
            printf("%s%s\n", "RHYTHM,", (*note_arr) -> rlit);
            printf("%s%s\n", "OCTAVE,", (*note_arr) -> tlit); */

            add_note((&note_arr[i]), mf, 1);
        }

        midiFileClose(mf);
    }
}

void bplay_note_arr(struct note *note_arr[], int beat){
    MIDI_FILE *mf;
    if ((mf = midiFileCreate("helloarraybeat.mid", TRUE))){
        midiSongAddTempo(mf, 1, beat);
        while (*note_arr){
            add_note((*note_arr), mf, 1);
            note_arr++;
        }
        midiFileClose(mf);
    }  
}

void iplay_note_arr(struct note *note_arr[], int instrument){
    MIDI_FILE *mf;
    if ((mf = midiFileCreate("helloarrayinst.mid", TRUE))){
        midiTrackAddProgramChange(mf, 1, instrument);  
        while (*note_arr){
            add_note((*note_arr), mf, 1);
            note_arr++;
        }
        midiFileClose(mf);
    }  
}

void add_note(struct note *note_ptr, MIDI_FILE *mf, int track){

    char *tlit = note_ptr -> tlit;
    int olit = note_ptr -> olit;
    char *rlit = note_ptr -> rlit;

    int miditone = 0;
    int is_rest = 0;
    char tone = tlit[0];
    printf("tone: %s\n", tlit);
    printf("octave: %d\n", olit);
    printf("rhythm: %s\n", rlit);
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
    midiSongAddSimpleTimeSig(mf, track, 4, MIDI_NOTE_CROCHET); 
    midiFileSetTracksDefaultChannel(mf, track, MIDI_CHANNEL_1); 
    midiTrackAddText(mf, track, textLyric, tlit);

    int volume = MIDI_VOL_MEZZO;
    if (is_rest){
        volume = 0;
    }

    midiTrackAddNote(mf, track, miditone + midioctave, midirhythm, volume, TRUE, FALSE);

}

void add_track(struct note *note_arr[], MIDI_FILE *mf, int track){

    printf("%d", track);
    while (*note_arr){
        add_note((*note_arr), mf, track);
        note_arr++;
    }
}

void play_tracks(int num_tracks, ...){
    MIDI_FILE *mf;
    va_list valist;
    va_start(valist, num_tracks);
    struct note **track;
    int i;

    if ((mf = midiFileCreate("playTracks.mid", TRUE))){
        
        for (i = 0; i < num_tracks; i++){
            track = va_arg(valist, struct note **);
            add_track(track, mf, i);
        }
        
        midiFileClose(mf);
    }
}

/* char **change_tone(char *tlit, int incr, int is_lower){

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
    else {printf("%s\n", "This is not an allowable accidental value.");}

    if(is_lower){miditone = miditone + accidental - incr;}
    else{miditone = miditone + accidental + incr;}

    miditone = ((miditone)%12+12)%12;

    char *toneMap[] = {"C", "C+", "D", "D+", "E", "F", "F+", "G", "G+", "A", "A+", "B"};

    return &toneMap[miditone];
} */

void change_tone(struct note *note, int incr, int is_lower){

    char *tlit = note -> tlit;
    int olit = note -> olit;

    /* char *newtlit;
    int newolit;
    char *samerlit = note -> rlit;
    struct note* newnoteptr;
    struct note newnote; */

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
    else {printf("%s\n", "This is not an allowable accidental value.");}

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

    if (is_rest) {tlit = tlit;}

    char *toneMap[] = {"C", "C+", "D", "D+", "E", "F", "F+", "G", "G+", "A", "A+", "B"};

    /* strcmp(newtlit, toneMap[miditone]);
    newolit = olit;
    newnoteptr = new_note(newtlit, newolit, samerlit);
    newnote = *newnoteptr; */

    strcpy(tlit, toneMap[miditone]);
    
    note -> tlit = tlit;
    note -> olit = olit;

}