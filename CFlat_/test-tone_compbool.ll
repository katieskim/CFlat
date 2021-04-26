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
@tone_ptr.9 = private unnamed_addr constant [3 x i8] c"A+\00"
@tone_ptr.10 = private unnamed_addr constant [3 x i8] c"A+\00"
@tone_ptr.11 = private unnamed_addr constant [3 x i8] c"G+\00"
@tone_ptr.12 = private unnamed_addr constant [3 x i8] c"A-\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

declare i32 @play_note(%named_struct_note_t*)

declare i32 @bplay_note(%named_struct_note_t*, i32)

declare %named_struct_note_t @change_tone(%named_struct_note_t*, i32, i32)

define i32 @main() {
entry:
  %n = alloca %named_struct_note_t
  %m = alloca %named_struct_note_t
  %t = alloca i8*
  %v = alloca i8*
  %o = alloca i8*
  %p = alloca i8*
  store %named_struct_note_t { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tone_ptr, i32 0, i32 0), i32 4, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @rhythm_ptr, i32 0, i32 0) }, %named_struct_note_t* %n
  store %named_struct_note_t { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tone_ptr.7, i32 0, i32 0), i32 5, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @rhythm_ptr.8, i32 0, i32 0) }, %named_struct_note_t* %m
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tone_ptr.9, i32 0, i32 0), i8** %t
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tone_ptr.10, i32 0, i32 0), i8** %v
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tone_ptr.11, i32 0, i32 0), i8** %o
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tone_ptr.12, i32 0, i32 0), i8** %p
  %"@tone" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone = load i8*, i8** %"@tone"
  %"@tone1" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %m, i32 0, i32 0
  %.tone2 = load i8*, i8** %"@tone1"
  %tmp = icmp eq i8* %.tone, %.tone2
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp)
  %"@tone3" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone4 = load i8*, i8** %"@tone3"
  %"@tone5" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %m, i32 0, i32 0
  %.tone6 = load i8*, i8** %"@tone5"
  %tmp7 = icmp ne i8* %.tone4, %.tone6
  %printf8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp7)
  %t9 = load i8*, i8** %t
  %v10 = load i8*, i8** %v
  %tmp11 = icmp eq i8* %t9, %v10
  %printf12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp11)
  %t13 = load i8*, i8** %t
  %v14 = load i8*, i8** %v
  %tmp15 = icmp ne i8* %t13, %v14
  %printf16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp15)
  %o17 = load i8*, i8** %o
  %p18 = load i8*, i8** %p
  %tmp19 = icmp eq i8* %o17, %p18
  %printf20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp19)
  %o21 = load i8*, i8** %o
  %p22 = load i8*, i8** %p
  %tmp23 = icmp ne i8* %o21, %p22
  %printf24 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp23)
  ret i32 0
}
