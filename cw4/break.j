
.class public break.break
.super java/lang/Object

.method public static write(I)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 0
    invokevirtual java/io/PrintStream/print(I)V
    return
.end method

.method public static read()I
    .limit locals 1
    .limit stack 3

    new java/util/Scanner          ; create new Scanner
    dup
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V

    invokevirtual java/util/Scanner/nextInt()I   ; read int
    ireturn
.end method


.method public static main([Ljava/lang/String;)V
    .limit locals 200
    .limit stack 200

  ;COMPILED CODE STARTS 

   ldc 0
   istore 0
For_Loop_begin_1:
   iload 0
   ldc 10
   if_icmpgt For_Loop_end_2
   iload 0
   invokestatic break/break/write(I)V
   getstatic java/lang/System/out Ljava/io/PrintStream;
   ldc "\n"
   invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
   iload 0
   ldc 1
   iadd
   istore 0
   goto For_Loop_begin_1
For_Loop_end_2:
   ldc 0
   istore 0
For_Loop_begin_3:
   iload 0
   ldc 10
   if_icmpgt For_Loop_end_4
   iload 0 		; i
   ldc 4
   if_icmple If_else_5
   goto For_Loop_end_4
   goto If-end_6
If_else_5:
If-end_6:
   iload 0
   invokestatic break/break/write(I)V
   getstatic java/lang/System/out Ljava/io/PrintStream;
   ldc "\n"
   invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
   iload 0
   ldc 1
   iadd
   istore 0
   goto For_Loop_begin_3
For_Loop_end_4:
   getstatic java/lang/System/out Ljava/io/PrintStream;
   ldc "Should print\n"
   invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
   goto ProgramEnd_0
   getstatic java/lang/System/out Ljava/io/PrintStream;
   ldc "Should not print\n"
   invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
ProgramEnd_0:

    ;COMPILED CODE ENDS

    return
    .end method