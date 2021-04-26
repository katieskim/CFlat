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
@tone_ptr.7 = private unnamed_addr constant [2 x i8] c"G\00"
@rhythm_ptr.8 = private unnamed_addr constant [2 x i8] c"q\00"
@tone_ptr.9 = private unnamed_addr constant [2 x i8] c"F\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

declare i32 @play_note(%named_struct_note_t*)

declare i32 @bplay_note(%named_struct_note_t*, i32)

declare %named_struct_note_t @change_tone(%named_struct_note_t*, i32, i32)

define i32 @main() {
entry:
  %n = alloca %named_struct_note_t
  %m = alloca %named_struct_note_t
  %a = alloca %named_struct_note_t
  %t = alloca i8*
  %v = alloca i8*
  store %named_struct_note_t { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tone_ptr, i32 0, i32 0), i32 4, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @rhythm_ptr, i32 0, i32 0) }, %named_struct_note_t* %n
  store %named_struct_note_t { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tone_ptr.7, i32 0, i32 0), i32 5, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @rhythm_ptr.8, i32 0, i32 0) }, %named_struct_note_t* %m
  %"@tone" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone = load i8*, i8** %"@tone"
  store i8* %.tone, i8** %t
  %t1 = load i8*, i8** %t
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.3, i32 0, i32 0), i8* %t1)
  %"@tone2" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @tone_ptr.9, i32 0, i32 0), i8** %"@tone2"
  %"@tone3" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %n, i32 0, i32 0
  %.tone4 = load i8*, i8** %"@tone3"
  %printf5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.3, i32 0, i32 0), i8* %.tone4)
  %t6 = load i8*, i8** %t
  %printf7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.3, i32 0, i32 0), i8* %t6)
  %"@tone8" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %m, i32 0, i32 0
  %.tone9 = load i8*, i8** %"@tone8"
  store i8* %.tone9, i8** %v
  %v10 = load i8*, i8** %v
  %printf11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.3, i32 0, i32 0), i8* %v10)
  %change_tone = call %named_struct_note_t @change_tone(%named_struct_note_t* %m, i32 1, i32 0)
  store %named_struct_note_t %change_tone, %named_struct_note_t* %m
  store %named_struct_note_t %change_tone, %named_struct_note_t* %a
  %"@tone12" = getelementptr inbounds %named_struct_note_t, %named_struct_note_t* %a, i32 0, i32 0
  %.tone13 = load i8*, i8** %"@tone12"
  %printf14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @fmt.3, i32 0, i32 0), i8* %.tone13)
  ret i32 0
}
