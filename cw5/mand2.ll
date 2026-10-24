
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
   %tmp_240 = load i32, i32* @Maxiters
   %tmp_239 = icmp sle i32  %tmp_240, %m
   br i1 %tmp_239, label %if_branch_258, label %else_branch_259

if_branch_258:
   call void @print_char (i32 32)
   ret void

else_branch_259:
   %tmp_244 = fmul double  %zi, %zi
   %tmp_245 = fmul double  %zr, %zr
   %tmp_243 = fadd double  %tmp_244, %tmp_245
   %tmp_242 = fcmp ole double  4.0, %tmp_243
   br i1 %tmp_242, label %if_branch_260, label %else_branch_261

if_branch_260:
   %tmp_248 = srem i32  %m, 10
   %tmp_247 = add i32  48, %tmp_248
   call void @print_char (i32 %tmp_247)
   ret void

else_branch_261:
   %tmp_249 = add i32  %m, 1
   %tmp_252 = fmul double  %zr, %zr
   %tmp_253 = fmul double  %zi, %zi
   %tmp_251 = fsub double  %tmp_252, %tmp_253
   %tmp_250 = fadd double  %x, %tmp_251
   %tmp_256 = fmul double  %zr, %zi
   %tmp_255 = fmul double  2.0, %tmp_256
   %tmp_254 = fadd double  %tmp_255, %y
   call void @m_iter (i32 %tmp_249, double %x, double %y, double %tmp_250, double %tmp_254)
   ret void
}

define void @x_iter (double %x, double %y) {
   %tmp_263 = load double, double* @Xmax
   %tmp_262 = fcmp ole double  %x, %tmp_263
   br i1 %tmp_262, label %if_branch_268, label %else_branch_269

if_branch_268:
   call void @m_iter (i32 0, double %x, double %y, double 0.0, double 0.0)
   %tmp_266 = load double, double* @Xstep
   %tmp_265 = fadd double  %x, %tmp_266
   call void @x_iter (double %tmp_265, double %y)
   ret void

else_branch_269:
   call void @skip ()
   ret void
}

define void @y_iter (double %y) {
   %tmp_271 = load double, double* @Ymax
   %tmp_270 = fcmp ole double  %y, %tmp_271
   br i1 %tmp_270, label %if_branch_278, label %else_branch_279

if_branch_278:
   %tmp_272 = load double, double* @Xmin
   call void @x_iter (double %tmp_272, double %y)
   call void @print_char (i32 10)
   %tmp_276 = load double, double* @Ystep
   %tmp_275 = fadd double  %y, %tmp_276
   call void @y_iter (double %tmp_275)
   ret void

else_branch_279:
   call void @skip ()
   ret void
}

define i32 @main() {
   %tmp_280 = load double, double* @Ymin
   call void @y_iter (double %tmp_280)
   ret i32 0
}

