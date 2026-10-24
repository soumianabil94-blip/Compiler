
declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  %1 = call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @skip() #0 {
  ret void
}

@.str = private constant [3 x i8] c"%d\00"

define void @print_int(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
   ret void
}

@.char = private constant [3 x i8] c"%c\00"

define void @print_char(i32 %x) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.char, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
   ret void
}

@.str_s = private constant [3 x i8] c"%s\00"

define void @print_string(i8* %p) {
   %t0 = getelementptr [3 x i8], [3 x i8]* @.str_s, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i8* %p)
   ret void
}


; END OF BUILD-IN FUNCTIONS (prelude)


@str_238 = private unnamed_addr constant [8 x i8] c"Squares\00"

@Max = global i32 10 

define i32 @sqr (i32 %x) {
   %tmp_225 = mul i32  %x, %x
   ret i32 %tmp_225
}

define void @all (i32 %n) {
   %tmp_227 = load i32, i32* @Max
   %tmp_226 = icmp sle i32  %n, %tmp_227
   br i1 %tmp_226, label %if_branch_233, label %else_branch_234

if_branch_233:
   %tmp_229 = call i32 @sqr (i32 %n)
   call void @print_int (i32 %tmp_229)
   call void @new_line ()
   %tmp_231 = add i32  %n, 1
   call void @all (i32 %tmp_231)
   ret void

else_branch_234:
   call void @skip ()
   ret void
}

define i32 @main() {
   call void @print_string (i8* getelementptr inbounds ([8 x i8], [8 x i8]* @str_238, i32 0, i32 0))
   call void @new_line ()
   call void @all (i32 0)
   ret i32 0
}

