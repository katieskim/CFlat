/*The CFlat API. Codegen should call functions from this library */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef  __APPLE__
#include <malloc.h>
#endif
#include "midifile.h"

/*
void set_instrument(int instrument){   Do we want to take in a voice/track too?
}   */

void play_note(char *tlit, int octave, char *rlit)
{
MIDI_FILE *mf;

	if ((mf = midiFileCreate("helloworld.mid", TRUE)))
		{
		/*CONVERTING CFLAT TONE => MIDI RHYTHM*/
        int miditone;
        char tone = tlit[0];
        if (tone == 'C') {miditone = 0;}
        if (tone == 'D') {miditone = 2;}
        if (tone == 'E') {miditone = 4;}
        if (tone == 'F') {miditone = 5;}
        if (tone == 'G') {miditone = 7;}
        if (tone == 'A') {miditone = 9;}
        if (tone == 'B') {miditone = 11;}

        int accidental = 0;
        if (strlen(tlit) > 1) {
            char acc = tlit[1];
            if (acc == '-') {accidental = -1;} 
            else if (acc == '+') {accidental = 1;}
            else if (acc == '.') {accidental = 0;}
            else {printf("%s", "This is not an allowable tone value.");}
            miditone = (miditone + accidental)%12;             /*Accounts for any wraparound needed for B# or Cflat*/
        }


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
        else {printf("%s", "This is not an allowable rhythm value.");}


        if (dotted) { midirhythm += midirhythm/2; }  /* adds the dotted portion */

        /*CONVERTING CFLAT OCTAVE => MIDI OCTAVE*/
        int midioctave = octave; 
        if (octave >= 0 && octave <= 10){
            midioctave *= 12;
        }
        /*DEFAULT TIME SIGNATURE */
        midiSongAddTempo(mf, 1, 120);
        midiSongAddSimpleTimeSig(mf, 1, 4, MIDI_NOTE_CROCHET); 


		midiFileSetTracksDefaultChannel(mf, 1, MIDI_CHANNEL_1);
		midiTrackAddProgramChange(mf, 1, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);    /*We only want to set this as instrument if instrument is not set already*/
        midiTrackAddText(mf, 1, textLyric, tlit);

		midiTrackAddNote(mf, 1, miditone + midioctave, midirhythm, MIDI_VOL_MEZZO, TRUE, FALSE);
		midiFileClose(mf);
		}
}



int main(int argc, char* argv[])
{
    
    play_note("C-", 4, "h.");
	return 0;
}