#####B-标准库
这是ANSI标准定义的语言标准库的概述。标准库并不是C语言本身固有的一部分，但是支持标准C的环境会提供函数声明，类型，宏定义的库。对于有限制的功能组件或者很容易用其他函数合成的函数，我们就略过不谈；宽字符也不再考虑范围；一些本地化事宜就不去想了，国际化一些本地语言文化背景什么的。
标准库里面的函数，类型还有宏都是在标准头文件里声明的：
><assert.h> <float.h> <math.h> <stdarg.h> <stdlib.h>
><ctype.h> <limits.h> <setjmp.h> <stddef.h> <string.h>
><errno.h> <locale.h> <signal.h> <stdio.h> <time.h>

头文件是这样包含近源文件中的
`#include <header>`
头文件可以重复包含多次，包含的顺序也没有关系。
头文件的包含必须是在任何外部声明或者定义之外的，而且是要在任何声明的使用之前进行包含。
头文件不需要是一个源文件。
下划线开头的外部定义由库保留使用，还有那些下划线，大写字符，下划线的名字也是的。_A_.
#####B.1 输入和输出：<stdio.h>
定义在头文件stdio.h中的输入输出函数，类型和宏，占到整个库的将近三分之一。
流，是数据的起源或者是终点，他会关联到一个硬盘设备或者其他的外接设备。库支持文本流和二进制流，虽然在一些系统中，特别是Unix中，两者是相同的。文本流就是行的序列；每一行都是由零个或者躲着字符组成，由一个\n换行符结尾。环境可能需要将文本流和其他形式互相转换（比如讲‘\n‘映射成换行和回车）。二进制流是未处理的字节构成的序列。
通过打开一个流，会连接到一个文件或者设备；连接也会随着流的关闭而结束。打开一个文件会返回一个FILE类型对象的指针，这里面记录了控制流所需要的任何信息。没有歧义的情况下，我们说的文件指针和流是一个意思。
当一个程序开始执行，那么三个六就已经打开了，分别是stdin，stdout和stderr。
#####B.1.1 文件操作
下面的函数是用来处理文件的操作的。size_t是操作符sizeof所生成的无符号整数类型。
`FILE *fopen(const char *filename, const char *mode)`
fopen会打开一个文件，返回一个流，如果打开失败的话，就会返回NULL。mode的合法值包括：
r，打开文本文件以供读取
w，创建文件来写入；如果之前有内容，全部丢弃
a，追加；打开或者创建文本文件，写入到文件末尾
r+，打开文件进行更新（读取和写入）
w+，创建文件进行更新，之前的内容全部丢弃
a+，追加；打开或创建文件来更新，写入到文件末尾
更新模式准许对同一个文件进行读取和写入；在读和写之间必须调用fflush函数或者文件定位函数，反过来也是一样的。如果在模式字之后包括了字母b，就像rb或者w+b，那就表示是一个二进制文件。文件名的最大字符数是由FILENAME_MAX来限制指定的。一次性可以打开的最多文件数是由FOPEN_MAX指定。
`FILE *freopen(const char *filename, const char *mode, FILE *stream)`
freopen会以特定的模式打开一个文件，并且关联到一个指定的流。返回值是流，出错的话就返回NULL。freopen一般适用于改变文件关联，不关联到默认的stdin，stdout和stderr。
`int fflush(FILE *stream)`
在一个输出流中，fflush函数将缓冲好的但是还没有写入文件的数据，写到文件中去。再输入流中，效果是未定义的，写入错误的话会返回EOF，没问题就会返回0.fflush(NULL)会刷新所有的输出流。
`int fclose(FILE *stream)`
fclose函数会关闭流，在关闭之前会刷新流没有写入的数据，清楚任何没有读入的缓冲输入，释放自动分配的缓冲空间。有错误出现的话就会返回EOF，正常就返回0。
`int remove(const char *filename)`
remove函数会删除文件，之后再尝试打开这个文件就会失败。如果尝试失败的话就会返回非0的值。
`int rename(const char *oldname, const char *newname)`
rename函数对一个文件进行重命名；如果失败就会返回非零值。
`FILE *tmpfile(void)`
tmpfile函数会创建一个临时文件，模式是wb+，在文件关闭或者程序运行结束的时候，这个临时文件就会自动删除。tmpfile返回一个流，如果创建不了的话就会返回NULL。
`char *tmpnam(char s[L_tmpnam])`
tmpnam(NULL)会创建一个字符串，这个字符串不是现有文件的名字，之后返回一个指向内部静态数组的指针。tmpnam(s)将字符串存储在s中，也作为函数返回值返回；s必须是至少有L_tmpnam个字符的空间。tmpnam在被调用的时候每次都生成一个不一样的名字；程序执行过程中，创建名字数量的上限有TMP_MAX指定。tmpnam只是创建名字，而不是创建文件。
`int setvbuf(FILE *stream, char *buf, int mode, size_t size)`
setvbuf控制流的缓冲；在读，写或者任何操作之前都必须被调用。_IOFBF模式会进行全缓冲，_IOLBF模式会进行文本文件的行缓冲，_IONBF就不缓冲。如果buf不是NULL，就会被当做缓冲使用，否则的话就会分配一个缓冲。size定义的是缓冲的大小。setvbuf会对任何错误返回非零的值。
`void setbuf(FILE *stream, char *buf)`
如果buf是NULL，缓冲就会关闭流。否则，setbuf就等同于(void) setvbuf(stream, buf, _IOFBF, BUFSIZ)
#####B.1.2 格式化输出
函数printf提供的是格式化输出转换。
`int fprintf(FILE *stream, const char *format, ...)`
fprintf会在format的控制下，将转换输出到流当中。返回值就是写入的字符数，如果出错的话就返回负值。
格式化字符串包含了两种对象：一种是普通字符，他会被简单拷贝到输出流中，另一种是转换定义，每一个转换定义都会用后续的参数进行转换后输出到流中。每一个转换定义都是用百分号%开头用一个转换字符结尾。在百分号和转换字符之间可以存在一些字符：
`int printf(const char *format, ...)`
这个函数等于函数fprint(stdout, ...)。
`int sprintf(char *s, const char *format, ...)`
sprintf和printf函数是一样的，除了一点，它的输出是写入到一个字符串s中的，用字符'\0'来结尾。字符串s必须足够大。返回值的计算中不包括字符'\0'。
`int vprintf(const char *format, va_list arg)`
`int vfprintf(FILE *stream, const char *format, va_list arg)`
`int vsprintf(char *s, const char *format, va_list arg)`
vprintf,vsprintf,vfprintf，这三个函数就是对应的printf函数版本，变量列表是由arg来代替，这个变量列表由宏va_start来初始化，或许会有va_arg来调用。
#####B.1.3 格式化输入
函数scanf处理格式化输入转换
`int fscanf(FILE *stream, const char * format, ...)`
fscnf从流中读入，参照的是format格式化字符串的控制，将后面的参数转化赋值，每一个参数必须都是指针。format结束，函数就会返回。在转化之前除了任何错误，或者是到了文件尾部，fscanf就会返回错误。正常的情况下，会发安徽转化赋值的字符个数。
`int scanf(const char *format, ...)`
这个函数相当于函数fscanf(stdin, ...)。
`int sscanf(const char *s, const char *format, ...)`
sscanf(s, ...)等同于函数scanf(...)，只是输入的字符是从字符串s中来的。
#####B.1.4 字符输入输出函数
`int fgetc(FILE *stream)`
fgetc将流的下一个字符作为无符号整数返回，出错就返回EOF。
`char *fgets(char *s, int n, FILE *stream)`
fgets读入流中的n-1个字符，传到数组s中，如果遇到换行就停止；换行符被保存在数组中，用'\0'结尾。
`int fputc(int c, FILE *stream)`
fputc将字符c写入到流stream中，出错或或者EOF就会返回负值。
`int fputs(const char *s, FILE *stream)`
fputs将字符串s写入到流中，出错返回负值，文件末尾返回EOF。
`int getc(IFLE *stream)`
getc和fgetc是相同的，但是getc是一个宏，会对stream不止求值一次。
`int getchar(void)`
getchar等同于getc(stdin)。
`char *gets(char *)`
gets会读入输入的行到数组s中；用'\0'来替代换行符，返回值是s，如果出错就返回NULL。
`int putc(int c, FILE *stream)`
putc和fputc是一样的，除了他是一个宏，对stream也会求值多次。
`int putchar(int c)`
putchar(c)等同于putc(c, stdout)
`int puts(const chatr *s)`
puts会将字符串s和一个换行符输出到stdout。如果错误发生就返回EOF，不然就是非负值。
`int ungetc(int c, FILE *stream)`
ungetc将字符c写回到流stream中。
#####B.1.5 直接输入输出函数
`size_t fread(void *ptr, size_t size, size_t nobj, FILE *stream)`
fread会从流stream中读取最多nobj个size大小的对象，存到数组ptr中。fread返回的是对象的数目，可能会比请求的数目要少。必须要用feofheferror函数才能查看状态。
`size_t fwrite(const void *ptr, size_t size, size_t nobj, FILE *stream)`
fwrite把数组ptr中的nobj个大小为size的对象写到流stream中。返回的是对象的写入数目。
#####B.1.6 文件定位函数
`int fseek(FILE *stream, long offset, int origin)`
fseek是为流stream来这只文件位置；后续的读写操作都会从新的位置开始。对二进制文件来说，位置，就是从origin开始的偏移量offset，origin的值可以使SEEK_SET(文件开始)，SEEK_CUR（当前位置），SEEK_END（文件末尾）。对于文本流，offset必须是0，或者是一个ftell函数返回的值（在ftell函数中，origin必须是SEEK_SET）。fseek会在错误的时候返回非零。
`long ftell(FILE *stream)`
ftell返回的是流stream的当前文件位置，出错的话返回-1。
`void rewind(FILE *stream)`
rewind(fp)等价于函数fseek(fp, 0L, SEEK_SET)；clearerr(fp)
`int fgetpos(FILE *stream, fpos_t *ptr)`
fgetpos会将流stream的当前位置记录到*ptr中，以供之后fsetpos函数使用。fpos_t类型是记录这个值的合适类型，fgetpos出错时会返回一个非零值。
`int fsetpos(FILE *stream, const fpos_t *ptr)`
fsetpos将流stream定位到fgetpos中的*ptr的位置，出错就会返回非零值。
#####B.1.7 错误处理函数
很多库函数都会在出错或者遇到文件末尾的时候设置状态显示器。这些显示器也可以显式地设置和测试。林外，整数表达式errno（在头文件<errno.h>中声明）会包一些常会出现的错误的具体信息。
`void clearerr(FILE *stream)`
clearerr会清除与流相关的EOF和错误显示。
`int feof(FILE *stream)`
feof设置流stream的EOF，返回一个非零的值。
`int ferror(FILE *stream)`
ferror设置流stream的错误显示，返回一个非零的值。
`void perror(const char *s)`
perror(s)会打印s还有一个定义好了的错误信息，这个信息是和错误号errno对应的就像下面的代码
`fprintf(stderr, "%s: %s\n", s, "error message");`
