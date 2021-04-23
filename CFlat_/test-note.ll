; ModuleID = 'CFlat'
source_filename = "CFlat"

%named_struct_note_t = type { i8*, i32, i8* }

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.5 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.6 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tone_ptr = private unnamed_addr constant [3 x i8] c"C-\00"
@rhythm_ptr = private unnamed_addr constant [3 x i8] c"s.\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %n = alloca %named_struct_note_t
  store %named_struct_note_t { i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tone_ptr, i32 0, i32 0), i32 4, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @rhythm_ptr, i32 0, i32 0) }, %named_struct_note_t* %n
  %n1 = load %named_struct_note_t, %named_struct_note_t* %n
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), %named_struct_note_t %n1)
  ret i32 0
}
