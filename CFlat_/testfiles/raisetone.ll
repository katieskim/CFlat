; ModuleID = 'raisetone.c'
source_filename = "raisetone.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

%struct.note = type { [3 x i8], i32, [3 x i8] }

@.str = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.1 = private unnamed_addr constant [26 x i8] c"This is not a valid tone.\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c"C\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"C+\00", align 1
@.str.4 = private unnamed_addr constant [2 x i8] c"D\00", align 1
@.str.5 = private unnamed_addr constant [3 x i8] c"D+\00", align 1
@.str.6 = private unnamed_addr constant [2 x i8] c"E\00", align 1
@.str.7 = private unnamed_addr constant [2 x i8] c"F\00", align 1
@.str.8 = private unnamed_addr constant [3 x i8] c"F+\00", align 1
@.str.9 = private unnamed_addr constant [2 x i8] c"G\00", align 1
@.str.10 = private unnamed_addr constant [3 x i8] c"G+\00", align 1
@.str.11 = private unnamed_addr constant [2 x i8] c"A\00", align 1
@.str.12 = private unnamed_addr constant [3 x i8] c"A+\00", align 1
@.str.13 = private unnamed_addr constant [2 x i8] c"B\00", align 1
@__const.change_tone.toneMap = private unnamed_addr constant [12 x i8*] [i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.4, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.5, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.6, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.7, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.8, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.9, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.10, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.11, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.12, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0)], align 16
@.str.14 = private unnamed_addr constant [2 x i8] c"h\00", align 1
@.str.15 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.16 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@note = common global %struct.note zeroinitializer, align 4
; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.note* @new_note(i8* %0, i32 %1, i8* %2) #0 {
  %4 = alloca %struct.note*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i8*, align 8
  %8 = alloca %struct.note*, align 8
  store i8* %0, i8** %5, align 8
  store i32 %1, i32* %6, align 4
  store i8* %2, i8** %7, align 8
  %9 = call i8* @malloc(i64 12) #6
  %10 = bitcast i8* %9 to %struct.note*
  store %struct.note* %10, %struct.note** %8, align 8
  %11 = load %struct.note*, %struct.note** %8, align 8
  %12 = icmp eq %struct.note* %11, null
  br i1 %12, label %13, label %14

13:                                               ; preds = %3
  store %struct.note* null, %struct.note** %4, align 8
  br label %33

14:                                               ; preds = %3
  %15 = load %struct.note*, %struct.note** %8, align 8
  %16 = getelementptr inbounds %struct.note, %struct.note* %15, i32 0, i32 0
  %17 = getelementptr inbounds [3 x i8], [3 x i8]* %16, i64 0, i64 0
  %18 = load i8*, i8** %5, align 8
  %19 = call i8* @__strcpy_chk(i8* %17, i8* %18, i64 3) #7
  %20 = load i32, i32* %6, align 4
  %21 = load %struct.note*, %struct.note** %8, align 8
  %22 = getelementptr inbounds %struct.note, %struct.note* %21, i32 0, i32 1
  store i32 %20, i32* %22, align 4
  %23 = load %struct.note*, %struct.note** %8, align 8
  %24 = getelementptr inbounds %struct.note, %struct.note* %23, i32 0, i32 2
  %25 = getelementptr inbounds [3 x i8], [3 x i8]* %24, i64 0, i64 0
  %26 = load i8*, i8** %7, align 8
  %27 = load %struct.note*, %struct.note** %8, align 8
  %28 = getelementptr inbounds %struct.note, %struct.note* %27, i32 0, i32 2
  %29 = getelementptr inbounds [3 x i8], [3 x i8]* %28, i64 0, i64 0
  %30 = call i64 @llvm.objectsize.i64.p0i8(i8* %29, i1 false, i1 true, i1 false)
  %31 = call i8* @__strcpy_chk(i8* %25, i8* %26, i64 %30) #7
  %32 = load %struct.note*, %struct.note** %8, align 8
  store %struct.note* %32, %struct.note** %4, align 8
  br label %33

33:                                               ; preds = %14, %13
  %34 = load %struct.note*, %struct.note** %4, align 8
  ret %struct.note* %34
}
; Function Attrs: allocsize(0)
declare i8* @malloc(i64) #1
; Function Attrs: nounwind
declare i8* @__strcpy_chk(i8*, i8*, i64) #2
; Function Attrs: nounwind readnone speculatable willreturn
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1 immarg, i1 immarg, i1 immarg) #3
; Function Attrs: noinline nounwind optnone ssp uwtable
define %struct.note* @change_tone(%struct.note* %0, i32 %1, i32 %2) #0 {
  %4 = alloca %struct.note*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i8*, align 8
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  %13 = alloca i8, align 1
  %14 = alloca [12 x i8*], align 16
  store %struct.note* %0, %struct.note** %4, align 8
  store i32 %1, i32* %5, align 4
  store i32 %2, i32* %6, align 4
  %15 = load %struct.note*, %struct.note** %4, align 8
  %16 = getelementptr inbounds %struct.note, %struct.note* %15, i32 0, i32 0
  %17 = getelementptr inbounds [3 x i8], [3 x i8]* %16, i64 0, i64 0
  store i8* %17, i8** %7, align 8
  %18 = load %struct.note*, %struct.note** %4, align 8
  %19 = getelementptr inbounds %struct.note, %struct.note* %18, i32 0, i32 1
  %20 = load i32, i32* %19, align 4
  store i32 %20, i32* %8, align 4
  store i32 0, i32* %9, align 4
  store i32 0, i32* %10, align 4
  %21 = load i8*, i8** %7, align 8
  %22 = getelementptr inbounds i8, i8* %21, i64 0
  %23 = load i8, i8* %22, align 1
  store i8 %23, i8* %11, align 1
  %24 = load i8, i8* %11, align 1
  %25 = sext i8 %24 to i32
  %26 = icmp eq i32 %25, 82
  br i1 %26, label %27, label %28

27:                                               ; preds = %3
  store i32 1, i32* %10, align 4
  br label %72

28:                                               ; preds = %3
  %29 = load i8, i8* %11, align 1
  %30 = sext i8 %29 to i32
  %31 = icmp eq i32 %30, 67
  br i1 %31, label %32, label %33

32:                                               ; preds = %28
  store i32 0, i32* %9, align 4
  br label %71

33:                                               ; preds = %28
  %34 = load i8, i8* %11, align 1
  %35 = sext i8 %34 to i32
  %36 = icmp eq i32 %35, 68
  br i1 %36, label %37, label %38

37:                                               ; preds = %33
  store i32 2, i32* %9, align 4
  br label %70

38:                                               ; preds = %33
  %39 = load i8, i8* %11, align 1
  %40 = sext i8 %39 to i32
  %41 = icmp eq i32 %40, 69
  br i1 %41, label %42, label %43

42:                                               ; preds = %38
  store i32 4, i32* %9, align 4
  br label %69

43:                                               ; preds = %38
  %44 = load i8, i8* %11, align 1
  %45 = sext i8 %44 to i32
  %46 = icmp eq i32 %45, 70
  br i1 %46, label %47, label %48

47:                                               ; preds = %43
  store i32 5, i32* %9, align 4
  br label %68

48:                                               ; preds = %43
  %49 = load i8, i8* %11, align 1
  %50 = sext i8 %49 to i32
  %51 = icmp eq i32 %50, 71
  br i1 %51, label %52, label %53

52:                                               ; preds = %48
  store i32 7, i32* %9, align 4
  br label %67

53:                                               ; preds = %48
  %54 = load i8, i8* %11, align 1
  %55 = sext i8 %54 to i32
  %56 = icmp eq i32 %55, 65
  br i1 %56, label %57, label %58

57:                                               ; preds = %53
  store i32 9, i32* %9, align 4
  br label %66

58:                                               ; preds = %53
  %59 = load i8, i8* %11, align 1
  %60 = sext i8 %59 to i32
  %61 = icmp eq i32 %60, 66
  br i1 %61, label %62, label %63

62:                                               ; preds = %58
  store i32 11, i32* %9, align 4
  br label %65

63:                                               ; preds = %58
  %64 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.1, i64 0, i64 0))
  br label %65

65:                                               ; preds = %63, %62
  br label %66

66:                                               ; preds = %65, %57
  br label %67

67:                                               ; preds = %66, %52
  br label %68

68:                                               ; preds = %67, %47
  br label %69

69:                                               ; preds = %68, %42
  br label %70

70:                                               ; preds = %69, %37
  br label %71

71:                                               ; preds = %70, %32
  br label %72

72:                                               ; preds = %71, %27
  store i32 0, i32* %12, align 4
  %73 = load i8*, i8** %7, align 8
  %74 = getelementptr inbounds i8, i8* %73, i64 1
  %75 = load i8, i8* %74, align 1
  store i8 %75, i8* %13, align 1
  %76 = load i8, i8* %13, align 1
  %77 = sext i8 %76 to i32
  %78 = icmp eq i32 %77, 45
  br i1 %78, label %79, label %80

79:                                               ; preds = %72
  store i32 -1, i32* %12, align 4
  br label %92

80:                                               ; preds = %72
  %81 = load i8, i8* %13, align 1
  %82 = sext i8 %81 to i32
  %83 = icmp eq i32 %82, 43
  br i1 %83, label %84, label %85

84:                                               ; preds = %80
  store i32 1, i32* %12, align 4
  br label %91

85:                                               ; preds = %80
  %86 = load i8, i8* %13, align 1
  %87 = sext i8 %86 to i32
  %88 = icmp eq i32 %87, 46
  br i1 %88, label %89, label %90

89:                                               ; preds = %85
  store i32 0, i32* %12, align 4
  br label %90

90:                                               ; preds = %89, %85
  br label %91

91:                                               ; preds = %90, %84
  br label %92

92:                                               ; preds = %91, %79
  %93 = load i32, i32* %6, align 4
  %94 = icmp ne i32 %93, 0
  br i1 %94, label %95, label %101

95:                                               ; preds = %92
  %96 = load i32, i32* %9, align 4
  %97 = load i32, i32* %12, align 4
  %98 = add nsw i32 %96, %97
  %99 = load i32, i32* %5, align 4
  %100 = sub nsw i32 %98, %99
  store i32 %100, i32* %9, align 4
  br label %107

101:                                              ; preds = %92
  %102 = load i32, i32* %9, align 4
  %103 = load i32, i32* %12, align 4
  %104 = add nsw i32 %102, %103
  %105 = load i32, i32* %5, align 4
  %106 = add nsw i32 %104, %105
  store i32 %106, i32* %9, align 4
  br label %107

107:                                              ; preds = %101, %95
  %108 = load i32, i32* %6, align 4
  %109 = icmp ne i32 %108, 0
  br i1 %109, label %110, label %120

110:                                              ; preds = %107
  br label %111

111:                                              ; preds = %114, %110
  %112 = load i32, i32* %9, align 4
  %113 = icmp slt i32 %112, 0
  br i1 %113, label %114, label %119

114:                                              ; preds = %111
  %115 = load i32, i32* %9, align 4
  %116 = add nsw i32 %115, 12
  store i32 %116, i32* %9, align 4
  %117 = load i32, i32* %8, align 4
  %118 = sub nsw i32 %117, 1
  store i32 %118, i32* %8, align 4
  br label %111

119:                                              ; preds = %111
  br label %130

120:                                              ; preds = %107
  br label %121

121:                                              ; preds = %124, %120
  %122 = load i32, i32* %9, align 4
  %123 = icmp sgt i32 %122, 11
  br i1 %123, label %124, label %129

124:                                              ; preds = %121
  %125 = load i32, i32* %9, align 4
  %126 = sub nsw i32 %125, 12
  store i32 %126, i32* %9, align 4
  %127 = load i32, i32* %8, align 4
  %128 = add nsw i32 %127, 1
  store i32 %128, i32* %8, align 4
  br label %121

129:                                              ; preds = %121
  br label %130

130:                                              ; preds = %129, %119
  %131 = bitcast [12 x i8*]* %14 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 16 %131, i8* align 16 bitcast ([12 x i8*]* @__const.change_tone.toneMap to i8*), i64 96, i1 false)
  %132 = load %struct.note*, %struct.note** %4, align 8
  %133 = getelementptr inbounds %struct.note, %struct.note* %132, i32 0, i32 0
  %134 = getelementptr inbounds [3 x i8], [3 x i8]* %133, i64 0, i64 0
  %135 = load i32, i32* %9, align 4
  %136 = sext i32 %135 to i64
  %137 = getelementptr inbounds [12 x i8*], [12 x i8*]* %14, i64 0, i64 %136
  %138 = load i8*, i8** %137, align 8
  %139 = call i8* @__strcpy_chk(i8* %134, i8* %138, i64 3) #7
  %140 = load i32, i32* %8, align 4
  %141 = load %struct.note*, %struct.note** %4, align 8
  %142 = getelementptr inbounds %struct.note, %struct.note* %141, i32 0, i32 1
  store i32 %140, i32* %142, align 4
  %143 = load %struct.note*, %struct.note** %4, align 8
  ret %struct.note* %143
}
declare i32 @printf(i8*, ...) #4
; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #5
; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main(i32 %0, i8** %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i8**, align 8
  %6 = alloca %struct.note*, align 8
  store i32 0, i32* %3, align 4
  store i32 %0, i32* %4, align 4
  store i8** %1, i8*** %5, align 8
  %7 = call %struct.note* @new_note(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.6, i64 0, i64 0), i32 4, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i64 0, i64 0))
  store %struct.note* %7, %struct.note** %6, align 8
  %8 = load %struct.note*, %struct.note** %6, align 8
  %9 = getelementptr inbounds %struct.note, %struct.note* %8, i32 0, i32 0
  %10 = getelementptr inbounds [3 x i8], [3 x i8]* %9, i64 0, i64 0
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.15, i64 0, i64 0), i8* %10)
  %12 = load %struct.note*, %struct.note** %6, align 8
  %13 = getelementptr inbounds %struct.note, %struct.note* %12, i32 0, i32 1
  %14 = load i32, i32* %13, align 4
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.16, i64 0, i64 0), i32 %14)
  %16 = load %struct.note*, %struct.note** %6, align 8
  %17 = call %struct.note* @change_tone(%struct.note* %16, i32 1, i32 0)
  ret i32 0
}

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind readnone speculatable willreturn }
attributes #4 = { "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { argmemonly nounwind willreturn }
attributes #6 = { allocsize(0) }
attributes #7 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [3 x i32] [i32 10, i32 15, i32 6]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 12.0.0 (clang-1200.0.32.29)"}
