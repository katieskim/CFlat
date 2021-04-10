#include <stdio.h>

struct Note {
	char* tone;
	int octave;
	char* rhythm;
};

int main() {
	struct Note note1;

	note1.tone = "C-";
	note1.octave = 4;
	note1.rhythm = "s.";
	
	return 0;
}
