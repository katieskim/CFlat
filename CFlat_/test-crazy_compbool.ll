; ModuleID = 'CFlat'
source_filename = "CFlat"

%named_struct_note_t = type { i8*, i32, i8* }

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [16 x i8] c"/%s/ /%d/ /%s/\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.3 = private unnamed_addr constant [6 x i8] c"/%s/\0A\00"
@fmt.4 = private unnamed_addr constant [6 x i8] c"/%d/\0A\00"
@fmt.5 = private unnamed_addr constant [6 x i8] c"/%s/\0A\00"
@fmt.6 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tone_ptr = private unnamed_addr constant [2 x i8] c"C\00"
@rhythm_ptr = private unnamed_addr constant [3 x i8] c"s.\00"
@tone_ptr.7 = private unnamed_addr constant [2 x i8] c"F\00"
@rhythm_ptr.8 = private unnamed_addr constant [2 x i8] c"q\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

declare i32 @play_note(%named_struct_note_t*)

declare i32 @bplay_note(%named_struct_note_t*, i32)

declare %named_struct_note_t @change_tone(%named_struct_note_t*, i32, i32)

define i32 @main() {
entry:
  %n = alloca %named_struct_note_t
  %m = alloca %named_struct_note_t
  store %named_struct_note_t { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tone_ptr, i32 0, i32 0), i32 4, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @rhythm_ptr, i32 0, i32 0) }, %named_struct_note_t* %n
  store %named_struct_note_t { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tone_ptr.7, i32 0, i32 0), i32 5, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @rhythm_ptr.8, i32 0, i32 0) }, %named_struct_note_t* %m
  %"@tone" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone = load i8*, i8** %"@tone"
  %"@tone1" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %m, i32 0, i32 0
  %.tone2 = load i8*, i8** %"@tone1"
  %tmp = icmp ne i8* %.tone, %.tone2
  %"@tone3" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone4 = load i8*, i8** %"@tone3"
  %"@tone5" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %m, i32 0, i32 0
  %.tone6 = load i8*, i8** %"@tone5"
  %tmp7 = icmp ne i8* %.tone4, %.tone6
  %tmp8 = and i1 %tmp, %tmp7
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp8)
  %"@tone9" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone10 = load i8*, i8** %"@tone9"
  %"@tone11" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %m, i32 0, i32 0
  %.tone12 = load i8*, i8** %"@tone11"
  %tmp13 = icmp ne i8* %.tone10, %.tone12
  %"@tone14" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone15 = load i8*, i8** %"@tone14"
  %"@tone16" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %m, i32 0, i32 0
  %.tone17 = load i8*, i8** %"@tone16"
  %tmp18 = icmp eq i8* %.tone15, %.tone17
  %tmp19 = or i1 %tmp13, %tmp18
  %printf20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp19)
  ret i32 0
}
