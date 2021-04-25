#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#ifndef  __APPLE__
#include <malloc.h>
#endif

#include "midifile.h"
#include "cflatapi.h"

int main(int argc, char* argv[]){

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
/*
    play_note(c, "plays_note");
    play_note(c_flat, "plays_note2");
    bplay_note(c, 48, "bplays_note");
    iplay_note(c, 100, "plays_note_instrument");

    char *name = "plays_note_arr";

    play_note_arr(arr,name);
    iplay_note_arr(arr, 20, "plays_arr_instrumental");*/

    struct note *A[4];  A[3] = NULL;
    struct note *B[4];  B[3] = NULL;
    struct note *C[5];  C[4] = NULL;
    struct note *D[5];  D[4] = NULL;

    for (i=0; i<3; i++){
        A[i] = malloc(sizeof(struct note*));
        B[i] = malloc(sizeof(struct note*));
        C[i] = malloc(sizeof(struct note*));
    }
    C[3] = malloc(sizeof(struct note*));
    D[3] = malloc(sizeof(struct note*));
     
    struct note *et = new_note("E", 4, "h");
    struct note *dt = new_note("D", 4, "h");
    struct note *ct = new_note("C", 4, "h");

    struct note *gt = new_note("G", 4, "h");
    struct note *ft = new_note("F", 4, "h.");

    struct note *ctt = new_note("C", 5, "h");
    struct note *dtt = new_note("D", 5, "w.");
    struct note *ett = new_note("E", 5, "h");
    struct note *at = new_note("A", 4, "h");

    A[0] = et;
    A[1] = dt;
    A[2] = ct;

    B[0] = gt;
    B[1] = ft;
    B[2] = et;

    C[0] = ctt;
    C[1] = at;
    C[2] = gt;
    C[3] = ctt;

    D[0] = ctt;
    D[1] = dtt;
    D[2] = ctt;
    D[3] = ett;

    /*play_tracks(4, A, B, C, D);*/

    printf("%s", at->tlit);
    printf("%d\n", at->olit);
    change_octave(at, 1, 0);

    printf("%d\n", at->olit);

    change_octave(at, 2, 0);

    printf("%d\n", at->olit);

    change_octave(at, 5, 1);

    printf("%d\n", at->olit);

    change_octave(at, 42, 0);

    printf("%d\n", at->olit);
	return 0;



}