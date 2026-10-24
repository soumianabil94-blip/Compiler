
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



@Ymin = global double -1.3 

@Ymax = global double 1.3 

@Ystep = global double 0.05 

@Xmin = global double -2.1 

@Xmax = global double 1.1 

@Xstep = global double 0.02 

@Maxiters = global i32 1000 

define void @m_iter (i32 %m, double %x, double %y, double %zr, double %zi) {
   %tmp_185 = load i32, i32* @Maxiters
   %tmp_184 = icmp sle i32  %tmp_185, %m
   br i1 %tmp_184, label %if_branch_201, label %else_branch_202

if_branch_201:
   call void @print_star ()
   ret void

else_branch_202:
   %tmp_189 = fmul double  %zi, %zi
   %tmp_190 = fmul double  %zr, %zr
   %tmp_188 = fadd double  %tmp_189, %tmp_190
   %tmp_187 = fcmp ole double  4.0, %tmp_188
   br i1 %tmp_187, label %if_branch_203, label %else_branch_204

if_branch_203:
   call void @print_space ()
   ret void

else_branch_204:
   %tmp_192 = add i32  %m, 1
   %tmp_195 = fmul double  %zr, %zr
   %tmp_196 = fmul double  %zi, %zi
   %tmp_194 = fsub double  %tmp_195, %tmp_196
   %tmp_193 = fadd double  %x, %tmp_194
   %tmp_199 = fmul double  %zr, %zi
   %tmp_198 = fmul double  2.0, %tmp_199
   %tmp_197 = fadd double  %tmp_198, %y
   call void @m_iter (i32 %tmp_192, double %x, double %y, double %tmp_193, double %tmp_197)
   ret void
}

define void @x_iter (double %x, double %y) {
   %tmp_206 = load double, double* @Xmax
   %tmp_205 = fcmp ole double  %x, %tmp_206
   br i1 %tmp_205, label %if_branch_211, label %else_branch_212

if_branch_211:
   call void @m_iter (i32 0, double %x, double %y, double 0.0, double 0.0)
   %tmp_209 = load double, double* @Xstep
   %tmp_208 = fadd double  %x, %tmp_209
   call void @x_iter (double %tmp_208, double %y)
   ret void

else_branch_212:
   call void @skip ()
   ret void
}

define void @y_iter (double %y) {
   %tmp_214 = load double, double* @Ymax
   %tmp_213 = fcmp ole double  %y, %tmp_214
   br i1 %tmp_213, label %if_branch_221, label %else_branch_222

if_branch_221:
   %tmp_215 = load double, double* @Xmin
   call void @x_iter (double %tmp_215, double %y)
   call void @new_line ()
   %tmp_219 = load double, double* @Ystep
   %tmp_218 = fadd double  %y, %tmp_219
   call void @y_iter (double %tmp_218)
   ret void

else_branch_222:
   call void @skip ()
   ret void
}

define i32 @main() {
   %tmp_223 = load double, double* @Ymin
   call void @y_iter (double %tmp_223)
   ret i32 0
}

