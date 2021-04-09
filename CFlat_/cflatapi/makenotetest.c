#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef  __APPLE__
#include <malloc.h>
#endif
#include "midifile.h"

void TestNote(void)
{
MIDI_FILE *mf;

	if ((mf = midiFileCreate("testnote.mid", TRUE)))
		{
		/* char *sing[] = {"Doh"};*/
		int c_flat = MIDI_NOTE_B;
        /*int d = MIDI_OCTAVE_3+MIDI_NOTE_D;
        int f = MIDI_OCTAVE_3+MIDI_NOTE_F_SHARP;*/

		/* Write tempo information out to track 1. Tracks actually start from zero
		** (this helps differentiate between channels, and ease understanding)
		** although, I'll use '1', by convention.
		*/
		 /* midiSongAddTempo(mf, 1, 120); */


		/* All data is written out to _tracks_ not channels. We therefore
		** set the current channel before writing data out. Channel assignments
		** can change any number of times during the file, and affect all
		** tracks messages until it is changed. */
		/*midiFileSetTracksDefaultChannel(mf, 1, MIDI_CHANNEL_1); */
        
		midiTrackAddProgramChange(mf, 1, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);
        /*midiTrackAddProgramChange(mf, 2, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);
        midiTrackAddProgramChange(mf, 3, MIDI_PATCH_ACOUSTIC_GRAND_PIANO);*/

		/* common time: 4 crochet beats, per bar */
		midiSongAddSimpleTimeSig(mf, 1, 4, MIDI_NOTE_QUARTER); 

		
	   /* midiTrackAddText(mf, 1, textLyric, sing[0]); */
		midiTrackAddNote(mf, 1, c_flat+60, 1000, MIDI_VOL_MEZZO, TRUE, FALSE);
        /*midiTrackAddNote(mf, 2, d, MIDI_NOTE_QUARTER, MIDI_VOL_MEZZO, TRUE, FALSE);
        midiTrackAddNote(mf, 3, f, MIDI_NOTE_QUARTER, MIDI_VOL_PIANO, TRUE, FALSE);
        midiTrackAddNote(mf, 2, f, MIDI_NOTE_QUARTER, MIDI_VOL_PIANO, TRUE, FALSE);
        midiTrackAddNote(mf, 4, f, MIDI_NOTE_QUARTER, MIDI_VOL_MEZZO_PIANO, TRUE, FALSE);*/

		midiFileClose(mf);
		}
}

int main(int argc, char* argv[])
{
	TestNote();
	return 0;
}