@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

@.num = global i32 10

define void @print_int(i32 noundef %0) {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  %4 = call i32 @identity(i32 noundef %3)
  %5 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %4)
  ret void
}

declare i32 @printf(ptr noundef, ...)

define internal i32 @identity(i32 noundef %0) {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}

define i32 @main() {
  %1 = load i32, ptr @.num
;  %2 = mul i32 %1, 10
  call i32 @print_int(i32 %1)
  ret i32 0
;  ret i32 %2
}

; Named metadata
!0 = !{i32 42, null, !"string"}
!foo = !{!0}
