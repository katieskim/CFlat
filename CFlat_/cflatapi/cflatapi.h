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
void play_note(struct note *note_ptr, char *filename);
void bplay_note(struct note *n, int beat, char *filename); /* bplay_note takes in beat (beats/min) */
void iplay_note(struct note *n, int instrument, char *filename);  /* iplay_note takes in an instrument 1-127*/


/* Plays a single note
**INPUT: Takes in a pointer to an array of note struct pointers
**OUTPUT: A midifile called "notearray.mid" that plays a C Major scale.
*/
void play_note_arr(struct note *note_arr[], char *filename);
void bplay_note_arr(struct note *note_arr[], int beat, char *filename); /* bplay_note_arr takes in beat (beats/min) */
void iplay_note_arr(struct note *note_arr[], int instrument, char *filename);
void ibplay_note_arr(struct note *note_arr[], int instrument, int beat, char *filename);

void play_tracks(int i, ...);


struct note* change_tone(struct note *note, int incr, int is_lower);
struct note* change_octave(struct note *note, int incr, int is_lower);
int is_tone_equal(struct note *a, struct note *b);
int is_note_equal(struct note *a, struct note *b);

#endif
