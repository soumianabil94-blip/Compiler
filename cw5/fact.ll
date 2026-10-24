
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



define i32 @fact (i32 %n) {
   %tmp_151 = icmp eq i32  %n, 0
   br i1 %tmp_151, label %if_branch_155, label %else_branch_156

if_branch_155:
   ret i32 1

else_branch_156:
   %tmp_153 = sub i32  %n, 1
   %tmp_154 = call i32 @fact (i32 %tmp_153)
   %tmp_152 = mul i32  %n, %tmp_154
   ret i32 %tmp_152
}

define i32 @facT (i32 %n, i32 %acc) {
   %tmp_157 = icmp eq i32  %n, 0
   br i1 %tmp_157, label %if_branch_161, label %else_branch_162

if_branch_161:
   ret i32 %acc

else_branch_162:
   %tmp_158 = sub i32  %n, 1
   %tmp_159 = mul i32  %n, %acc
   %tmp_160 = call i32 @facT (i32 %tmp_158, i32 %tmp_159)
   ret i32 %tmp_160
}

define i32 @facTi (i32 %n) {
   %tmp_163 = call i32 @facT (i32 %n, i32 1)
   ret i32 %tmp_163
}

define void @top () {
   %tmp_165 = call i32 @fact (i32 6)
   call void @print_int (i32 %tmp_165)
   call void @print_char (i32 44)
   %tmp_168 = call i32 @facTi (i32 6)
   call void @print_int (i32 %tmp_168)
   call void @print_char (i32 10)
   ret void
}

define i32 @main() {
   call void @top ()
   ret i32 0
}

