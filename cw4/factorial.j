
.class public factorial.factorial
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

   getstatic java/lang/System/out Ljava/io/PrintStream;
   ldc "Enter a number for n: "
   invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
   invokestatic factorial/factorial/read()I
   istore 0 		; n
   getstatic java/lang/System/out Ljava/io/PrintStream;
   ldc "Enter an initial value for the acc: "
   invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
   invokestatic factorial/factorial/read()I
   istore 1 		; acc
Loop_begin_1:
   iload 0 		; n
   ldc 0
   if_icmpeq Loop_end_2
   iload 1 		; acc
   iload 0 		; n
   imul
   istore 1 		; acc
   iload 0 		; n
   ldc 1
   isub
   istore 0 		; n
   goto Loop_begin_1
Loop_end_2:
   iload 1
   invokestatic factorial/factorial/write(I)V
ProgramEnd_0:

    ;COMPILED CODE ENDS

    return
    .end method