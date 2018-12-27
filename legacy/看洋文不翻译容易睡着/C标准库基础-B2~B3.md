#####B.2 字符类型测试：<ctype.h>
头文件<ctype.h>中声明的函数是用来测试字符的。对于每一个函数，参数列表都是一个int类型，这个int的值必须是EOF或者可以使用unsigned char来表示，所有函数的返回值也是int。如果参数c符合描述条件的话，函数就会返回非零值（true），如果不满足就会返回零。
isalnum(c) 意思是isalpha(c)为真或者isdigit(c)为真。
isalpha(c) 意思是isupper(c)为真或者islower(c)为真。
iscntrl(c) 控制字符
isdigit(c) 十进制数字
isgraph(c) 除了空格的打印字符
islower(c) 小写字母
isprint(c) 包括空格的打印字符
ispunct(c) 除了空格字母和数字外的打印字符
isspace(c) 空格，换页，换行，回车，横向纵向制表
isupper(c) 大写字母
isxdigit(c) 十六进制数
在七位的ASCII字符集中，打印字符包括了从0x20（' '）到0x7E（'-'）;控制字符是从0NUL开始到0x1F（US），还有0x7F(DEL)。
另外还有两个函数是用来转化字母大小写的：
int tolower(c) 将c转化成小写
int toupper(c) 将c转化成大写
#####B.3 字符串函数：<string.h>
在头文件string.h定义了两组函数。第一组函数是以str开头的，第二组函数是以mem开头的，除了函数memmove，没有定义重复对象的拷贝函数，比较函数使用的参数被冬枣unsigned char来处理。
接下来的函数中s和t都char *类型；cs和ct都是const char *类型；n是size_t类型；c是一个转成char的int。
`char *strcpy(char *s, const char *ct)`
将字符串ct拷贝到字符串s，包括最后的字符'\0';返回s。
`char *strncpy(char *s, const char *ct, size_t n)`
拷贝字符串ct的至多n个字符到s；返回s。如果ct的长度小于n则用'\0'填充。
`char *strcat(char *s, const char *ct)`
将字符串ct连接到字符串s的尾部；返回s。
`char *strncat(char *s, const cahr *ct, size_t n)`
将字符串ct的至多n个字符连接到字符串s的后面，由'\n'结尾；返回s。
`int strcmp(const char *cs, const char *ct)`
根据cs和ct的比较结果来决定返回值，cs<ct就返回小于0的值，cs==ct就返回0，cs>ct就返回大于0的值。
`int strcmp(const char *cs, const char *ct, int n)`
比较cs和ct的至多n个字符来决定返回值，cs<ct就返回小于0的值，cs==ct就返回0，cs>ct就返回大于0的值。
`char *strchr(const char *cs, char c)`
返回在字符串cs中第一次出现字符c的指针，如果没有出现就返回NULL。
`char *strrchr(const char *cs, char c)`
返回在字符串cs中最后一次出现字符c的指针，没出现就返回NULL。
`size_t strspn(const char *cs, const char *ct)`
返回在字符串cs中包含由ct的字符组成的前缀的长度。
`size_t strcspn(const char *cs, const char *ct)`
返回在字符串cs中不包含由ct的字符组成的前缀的长度。
`char *strpbrk(const char *cs, const char *ct)`
返回一个指针，指向的是在字符串ct中的任意一个字符出现在字符串cs中的位置，没出现就返回NULL。
`char *strstr(const char *cs, const char *ct)`
返回一个指针，指向的是字符串ct第一次出现在cs中的位置，如果cs不包含ct，就返回NULL。
`size_t strlen(const char *cs)`
返回cs的长度
`char *strerror(int n)`
返回指向定义字符串对应的错误n的指针。
`char *strtok(char *s, const char *ct)`
strtok分解字符串s，分界符就是字符串ct。
mem开头的函数意味着将对象最为字符数组进行处理；它的意图是做一个高效的函数接口。下面的函数中，s和t是void *类型；cs和ct是const void *类型；n是size_t类型；c是转化成unsigned char的int类型。
`void memcpy(char *s, const char *ct, int n)`
从ct中拷贝那个字符到s，返回s。
`void memmove(s, ct, n)`
和memcpy一样，除了一点，在两个对象有重叠的情况下，memmove依然有效。
`int memcmp(cs, ct, n)`
将cs的前n个字符和ct进行比较。
`void *memchr(cs, c, n)`
返回一个指针，指向字符c在字符串cs中第一次出现的位置，如果没有在前n个字符出现，返回null。
`void *memset(s, c, n)`
将字符c写进字符串s的前n个字符，返回s。


