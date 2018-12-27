#####B.4 数学函数：<math.h>
头文件math.h声明的是数学函数和宏。
在下面的函数中，x和y是double类型，n是int类型，所有的函数都返回double。三角函数的角度是用弧度表示法表示的。
sin(x) 		正弦函数
cos(x) 		余弦函数
tan(x) 		正切函数
asin(x) 	        反正弦函数
acos(x)		反余弦函数
atan(x)		反正切函数
atan2(x) 	二倍反正切函数
sinh(x) 	        双曲正弦函数
cosh(x) 	        双曲余弦函数
tanh(x) 	        双曲正切函数
exp(x) 		e的指数函数
log(x)		自然对数函数ln(x),x>0
log10(x)	        底数为10的对数函数
pow(x,y)	x的y次方
sqrt(x)		x的平方根
ceil(x)		不小于x的最小正整数
floor(x)	        不大于x的最大正整数
fabs(x)		x的绝对值
ldexp(x,n)	x乘以2的n次方
frexp(x, int *ip)	把一个浮点数分解为尾数和指数
modf(x, double *ip)		分解x，以得到x的整数和小数部分
fmod(x, y)		计算x对y的模，即x/y的求余运算
#####B.5 实用函数：<stdlib.h>
头文件stdlib.h中声明的函数是用来数字转换，存储分配，还有相似的功能。
`double atof(const char *s)`
atof将字符串s转化成double，等价于函数strtol(s, (char **)NULL)

`int atoi(const char *s)`
将s转化成int；等价于函数(int)strtol(s, (char **)NULL, 10)

`long atol(const char *s)`
将s转化成long；等价于函数strtol(s, (char **)NULL, 10)

`double strtod(const char *s, char **endp)`
strtod函数将字符串s的前缀转化成为double类型，并在转换时跳过s的前导空白符。
`long strtol(const char *s, char **endp, int base)`
strtol将字符串s的前缀转化成long，忽略前导空格。
`unsigned long strtoul(const char *s, char **endp, int base)`
和strtol一样，只是输出的是一个unsigned long类型。
`int rand(void)`
rand会返回一个伪随机数，范围是从0到RAND_MAX，上限至少是32767.
`void srand(unsigned int seed)`
srand使用see作为种子，形成一个新的伪随机数序列。初始化seed是1.
`void *calloc(size_t nobj, size_t size)`
calloc返回一个指针，指向的是为nobj随想的数组分配的空白空间，每一个大小都是size。如果请求没有满足就会返回NULL。空间初始化城0字节。
`void *malloc(size_t size)`
malloc返回的是一个对象size大小的空间的指针，如果失败就返回NULL，空间不进行初始化。
`void *realloc(void *p, size_t size)`
realloc将一个用p指向的对象的大小修改成size大小。
`void free(void *p)`
free会重新分配有p指向的空间；如果p是NULL就什么都不做。p必须是之前指向空间的指针，是由函数calloc，malloc或者realloc分配的。
`void abort(void)`
abort会导致程序的非正常终止，就像raise(SIGABRT)函数一样。
`void exit(int status)`
exit会导致程序的正常终止。atexit函数会被反过来注册调用，打开的文件会刷新，打开的流会关闭，控制权返回给环境。
`int atexit(coid (*fcn)(void))`
atexit注册函数调用的fcn，函数会正常结束。
`int system(const chat *s)`
system会传递一个字符串s来给系统环境执行。
`char *getenv(const char *name)`
getenv会返回跟那么关联的环境字符串，如果没有的话就返回NULL。
`void *bsearch(const void *key, const void *base, size_t n, size_t size, int (*cmp)(const void *keyval, const void *datum))`
bsearch在base[0]...base[n-1]之间查找与*key匹配的项

`void qsort(void *base, size_t n, size_t size, int (*cmp)(const void *, const void *))`
qsort函数对base[0]...base[n-1]数组中的对象进行升序排序，数组中每个对象长度为size。
`int abs(int n)`
abs函数返回int类型参数的绝对值
`long labs(long n)`
labs函数返回long类型参数n的绝对值
`div_t div(int num, int denom)`
div函数计算num/denom的商和余数，把结果保存在div_t类型的两个int类型成员quot和rem中。
`ldiv_t ldiv(long num, long denom)`
ldiv函数计算num/denom的商和余数，把结果保存在div_t类型的两个long类型成员quot和rem中。
#####B.6 诊断：<assert.h>
宏assert被用于给程序机上诊断信息
`void assert(int expression)`
如果expression是0，assert宏就会打印一个stderr信息，
`Assertion failed: expression, file filename, line nnn `
之后会调用abort来终结执行。filename和nnn来自于处理器宏__FILE__和__LINE__

