#ifndef CFLATAPI_H_
#define CFLATAPI_H_

/* Note struct */
struct note{
    char tlit[3];
    int olit;
    char rlit[3];
}note;
/*Allocates space for a note struct*/
struct note *new_note(char *tone, int octave, char *rhythm);

/*
** CFLAT USER FUNCTION DECLARATIONS
*/

/* Plays a single note
**INPUT: Takes in a pointer to a single note struct 
**OUTPUT: A midifile called "(i/b)hellonote.mid" that plays the note 
*/
void play_note(struct note *n); 
void bplay_note(struct note *n, int beat); /* bplay_note takes in beat (beats/min) */
void iplay_note(struct note *n, int instrument);  /* iplay_note takes in an instrument 1-127*/


/* Plays a single note
**INPUT: Takes in a pointer to an array of note struct pointers
**OUTPUT: A midifile called "notearray.mid" that plays a C Major scale.
*/
void play_note_arr(struct note *note_arr[]);
void bplay_note_arr(struct note *note_arr[], int beat); /* bplay_note_arr takes in beat (beats/min) */
void iplay_note_arr(struct note *note_arr[], int instrument);
void ibplay_note_arr(struct note *note_arr[], int instrument, int beat);

void play_tracks(int i, ...);

#endif
