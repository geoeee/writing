#####3.4 说说相等和内部表示
在Lisp中主要有5种相等断言，因为不是所有的对象被创建的时候都是相等意义上的相等。数字相等断言，=，测试两个数字是不是一样。当把=用在非数字上的时候，就会出错。其他的等于断言可以操作任何对象，但是要理解他们之间的区别，我们需要理解Lisp的内部表达方式。
当Lisp在不同的地方读取一个符号的时候，他的结果肯定是一样的符号。Lisp系统会会务一张符号表，提供read函数使用来定位字符和符号之间的映射。但是在不同的地方读取或者构建一个列表的时候，结果并不是完全意义上的相同，甚至对应的元素也是。这是因为read调用cons来构建列表，每一次对cons的调用都会返回一个新的组合单元。下图展示了两个列表x和y，他们都等于(one two)，但是他们是由不同的组合单元组成的，因此并不完全相同。下面的图片还展示了表达哈斯(rest x)并不会产生新的组合单元，而是和原来的结构共享x，所以表达式(cons ‘zero x)会生成一个新的组合单元，它的rest就是x。
![两个列表的相等性判断.JPG](http://upload-images.jianshu.io/upload_images/46495-c11600e51af31290.JPG)
当两个数学意义上相等的数字被读取或者被计算的时候，他们可能相等，也可能不相等，根据相关实现的设计者认为哪一个更有效率来决定。在大部分系统中，两个相等的定长数会是完全一样的，但是不同类型的数字并不相等（特别是短精度）。Common Lisp提供了一般用的四个相等断言。所有四个断言都是字母eq开头的，后面的字符更多意味着这个断言会考虑更多的对象。最简单的断言就是eq，用来测试严格相等的对象。而后是eql用来测试eq相等或者相等的数字。Equal测试那些eql相等的对象或者每一个元素eql相等的列表或者字符串。最后，equalp和equal类似，只是他会匹配大小写的字母与和不同类型的数字。下面的表格总结了四个相等断言的应用结果。问好的意思是结果根据实现不同而不同：两个eql相等的整型数可能eq并不相等。
![相等断言比较.JPG](http://upload-images.jianshu.io/upload_images/46495-cb21997ee4c9e816.JPG)
另外，还有一些特殊用途的想等断言，比如=，tree-equal，char-equal，和string-equal，是分别用来比较数字，树，字符和字符串的。
#####3.5 操作序列的函数
Common Lisp是一个处在过去的Lisp和未来的Lisp的过渡版本。这点上在序列操作的函数上最显而易见。最早期的Lisp值会处理符号，数字，列表，并且提供了像append和length这样的类表函数。更现代的Lisp版本加上了对于向量，字符串和其他数据结构的支持，并且引入了术语，序列来统一指称向量和列表。（一个向量就是一个一位数组。他比列表表现的更简洁，因为像狼不需要存储rest指针。获取第n个元素的操作上，向量比列表也更加高效，因为向量不需要遍历指针链）现代Lisp也支持字符向量组成的字符串，因此也是序列的一个子类型。
随着新的数据类型到来，对于操作这个类型的函数的命名问题也出现了。在一些情况下，Common Lisp选择沿用老的函数，length可以同时应用到列表和向量上。在其他情况下，老的名字会专门为列表函数保留，新的名字会为更加通用的序列函数发明出来。例如，append和mapcar只工作在列表上，，但是concatenate和map可以操作任意类型的序列。还有一些情况，会为新的数据结构发明新的函数。例如，有7个函数来取出一个序列中的第n个元素。最通用的就是elt，可以操作任意序列类型，但是也有特别的函数来操作列表，数组，字符串，比特向量，简单比特向量和简单向量。令人疑惑的是，nth函数是唯一一个将索引参数放在第一个参数的函数：
(nth n list)
(elt sequence n)
(aref array n)
(char string n)
(bit bit vector n)
(sbit simple-bit vector n)
(svref simple-vector n)
根据需要，最重要的序列函数列在本章的其他地方。
#####3.6 维护表格的函数
Lisp中列表可以用来表示一个一维的对象序列。由于列表丰富的特性，经常用作其他的用途，比如表示一张信息的表格。联合列表是用来实现表格的一种列表类型。一个联合列表就是一个点对的列表，每一对都由一个key和一个值组成。和在一起，这个点对的列表就形成了一张表格：给出一个key，我们就个可以根据这个key找出对应的值，或者核实这个表格中没有这样的key。下面是一个例子，用的是各个州的名字和他们的两字母缩写。使用的函数是assoc，返回的是key和值的对（如果有的话）。为了获得值，之后还要用cdr函数来处理assoc的结果。
`(setf state-table`
`‘((AL . Alabama) (AK . Alaska) (AZ . Arizona) (AR . Arkansas)))`
`>(assoc ‘AK state-table) => (AK . ALASKA)`
`>(cdr (assoc ‘AK state-table)) => ALASKA`
`>(assoc ‘TX state-table) => NIL`
如果想要根据值而不是key来检索列表，就要使用函数rassoc
`>(rassoc ‘Arizona table) => (AZ . ARIZONA)`
`> (car (rassoc 'Arizona table) => AZ`
使用assoc来管理一张表是很简单的，但是也有一个缺点；我们检索一个元素的时候，每一次都不得不搜索整张表格。如果列表很长的话，那么效率就不高。
另一种管理表格的方式是使用哈希表。这是一种专门高效管理大量数据的设计，但是对于规模较小的表格来说，它的开销不是能接受的。函数gethash和很多get开头的函数一样接受两个参数，一个key和一个值。表格本身使用一个对make-hash-table的调用来初始化，然后用一个使用gethash的setf来修改。
`(setf table (make-hash-table)`
`(setf (gethash 'AL table) 'Alabama) `
`(setf (gethash 'AK table) 'Alaska) `
`(setf (gethash 'AZ table) 'Arizona) `
`(setf (gethash 'AR table) 'Arkansas)`
接下来我们就可以这么获取值
`> (gethash 'AK table) => ALASKA `
`> (gethash 'TX table) => NIL`
函数remhash会从一个哈希表中删除一个键值对，clrhash会移除所有的键值对，还有maphash可以用来定位所有的键值对。哈希表的key是不收限制的，可以指向任何Lisp对象。还有更多的细节在Common Lisp的哈希表实现中，还有一个扩展的理论框架。
第三种表现表格的方式是用属性列表。一个属性列表就是一个可变的键值对列表。属性列表（有些时候叫做p-lists或者plists）和联合列表（a-lists或者alists）是很相似的：

a-list: (key1 . val1) (key2 . val2) … (keyn . valn))
p-list: (key1 val1 key2 val2 … keyn valn)
根据给定的条件，需要使用哪一种结构在两种结构间要做出选择。他们都是同一种信息的少许不同的表现形式。差别在于他们是如何被使用。每一个符号都有一个相关联的属性列表，这意味着我们可以将一个属性或者值对，直接关联在符号上。大部分程序只是用很少的不同属性但是对于每一个属性却有很多属性值对。因此，每一个符号的p-list会是比较短的。在我们例子中，我们只对一个属性感兴趣：每一个所写的关联州名。意思是，属性列表实际上非常简短，一个缩写只对应一个属性，而不是在关联列表中对应50个对的列表。
属性值可以用函数get获取，他接受两个参数，第一个是一个我们正在检索的信息的符号，第二个就是我们想要的符号的属性。Get返回属性的值，如果有的话。属性值对可以用一个setf的形式的符号来存储。一张表格可以构建如此下：
`(setf (get 'AL 'state) 'Alabama) `
`(setf (get 'AK 'state) 'Alaska) `
`(setf (get 'AZ 'state) 'Arizona) `
`(setf (get 'AR 'state) 'Arkansas)`
现在就可以用get来获取值：
`> (get 'AK 'state) => ALASKA `
`> (get 'TX 'state) => NIL`
这样子效率就提高了，因为我们可以直接从一个度好的单个属性中获取值，而不必在意拥有属性的富豪数量。然而，如果给定的符号有超过一个属性，之后我们还是不得不线性检索属性列表。请注意在属性列表中，没有雨rassoc对应功能的函数；如果你想获得一个州名的缩写，你可以存储一个缩写对应的州名属性，但是那会是一个独立的setf形式：
`(setf (get 'Arizona 'abbrev) 'AZ)`
事实上，当远，属性和值都是符号的时候，使用属性的可能就很小了。我们可以模仿a-list的方法，用一个符号列出所有的属性，在函数中使用setf来设置symbol-plist（会给出一个符号的完整属性列表）：
`(setf (symbol-plist ‘state-table)`
`'(AL Alabama AK Alaska AZ Arizona AR Arkansas))`
`> (get 'state-table 'AL) => ALASKA `
`> (get 'state-table 'Alaska) => NIL`
属性列表在Lisp中有很长的历史，但是哈希表被引入之后就开始失宠了。避免使用属性列表有两个主要的理由，第一，因为符号和他的属性列表是全局的，当要合并两个程序的时候很容易产生冲突，由于不同的目的的话，就不能一起工作。甚至一个符号在两个程序中使用不同属性的话，也会互相拖后腿。第二，属性列表是一团乱麻，如果表格的实现是属性列表，没有什么函数可以快速的移除元素。相对的，哈希表中有clrhash，或者将一个联合列表设置成nil。
#####3.7 操作树的函数
很多Common Lisp函数将表达式((a b) ((c)) (d e))看做一个三元素的序列，但是也有一些函数会把他看做一个有5个非空叶子节点的树。函数copy-tree会创建一个树的拷贝，函数tree-equal会测试两个树在组合单元层面是不是相等。这这方面tree-equal和equal相似，但是tree-equal更加强大，因为他允许：test关键字：
`>(setf tree ‘((a b) ((c)) (d e)))`
`>(tree-equal tree (copy-tree tree)) => T`
`(defun same-shape-tree (a b)`
`“Are two trees the same expect for the leaves?”`
`(tree-equal a b :test #’true))`
`(defun true (&rest ignore) t)`
`>(same-shape-tree tree ‘((1 2) ((3)) ( 4 5))) => T`
`>(same-shape-tree tree ‘((1 2) (3) (4 5))) => NIL`
下面的图片是显示((a b) ((c)) (d e))在组合单元层面的表现
![一棵树的组合单元形式.JPG](http://upload-images.jianshu.io/upload_images/46495-d4deb38371e45d38.JPG)
还有两个函数是将原来旧的表达式用一个新的表达式替换。Subst会替换一个简单的值，而sublis会用联合列表的形式(old . new)对，来替换列表。请注意在alist中旧值和新值的顺序，sublis和subst的参数顺序是反的。名字sublis是一个字符缩写，而且有些让人困惑，实际上更好的名字是subst-list。
`>(subst ‘new ‘old ‘(old ((very old)))) =>(New ((VERY NEW)))`
`>(sublis ‘((old . new)) ‘(old ((very old)))) => (NEW ((VERY NEW)))`
`>(subst ‘new ‘old ‘old) => ‘NEW`

(defun English->French (words)
 (sublis ‘((are . va) (book . libre) (friend . ami)
    (hello . bonjour) (how . cmment) (my . mon)
    (red . rouge) (you . tu))
 words))
`>(English->French ‘(hello my friend – how are you taoday?)) => (BONJOUR MON AMI –` `COMMENT VA TU TODAY?)`
#####3.8 操作数字的函数
最常用的操作数字的函数都列印在这里，不常用的函数就省略了。

表达式|计算结果|功能含义
---|---|---
(+ 4 2)|=>6|加法
(- 4 2)|=>2|减法
(* 4 2)|=>8|乘法
(/ 4 2)|=>2|除法
(> 100 99)|=>t|大于（>=大于等于）
(= 100 100)|=>t|等于（/=不等于）
(< 99 100)|=>t|小于（<=小于等于）
(random 100)|=>42|0到99 的随机数
(expt 4 2)|=>16|求幂（还有exp和log函数）
(sin pi)|=>0.0|sin函数（还有cos，tan等等）
(asin 0)|=>0.0|sin的反函数arcsin（还有acos，atan等等）
(min 2 3 4)|=>2|最小数（还有max）
(abs -3)|=>3|绝对值
(sqrt 4)|=>4|平方根
(round 4.1)|=>4|化为整数（还有truncate，floor，ceiling）
(rem 11 5)|=>1|余数（还有mod）

#####3.9 操作集合的函数
列表的重要用处之一就是表示集合。Common Lisp提供了函数来这样操作列表。例如，一般表现集合的r={a,b,c,d}和s={c,d,e}，我们可以这么做：
`> (setf r '(a b cd)) => (A BCD) `
`> (setf s '(c de)) => (C D E) `
`> (intersection r s) => (C D)`
有些实现会返回(C D)作为答案，另外一些实现会返回(D C)。相同的集合，都是合法的，你的程序也不应该依赖结果的元素顺序。下面是一些操作集合的主要函数

表达式|计算结果|功能含义
---|---|---
(intersection r s)|=>(c d)|两个集合的共有元素
(union r s)|=>(a b c d e)|两个集合所有的元素
(set-difference r s)|=>(a b)|在r中但是不在s中的元素
(member ‘d r)|=>(d)|检查一个元素是不是集合的成员
(subset s r)|=>nil|子集关系判断
(adjoin ‘b s)|=>(b c d e)|给集合加一个元素
(adjoin ‘c s)|=>(c d e)|加一个元素，但是不加重复元素

给定一个特殊的领域，将集合用比特位序列来体现也是可以的。例如，如果每一个集合(a b c d e)的子集合就是讨论的范围，那么我们就可以使用位序列11110来表示(a b c d)，00000来表示空集合，11001来表示(a b e)。比特位序列在Common Lisp中可以使用比特向量来表示，或者是一个整数的为禁止形式，例如，(a b e)可以用比特向量井号星号11011表示，或者用整数25表示，也可以写成井号b11001。
使用比特向量的好处就是节省集合编码的空间，当然是在一个预定的语境下面。计算会更加迅速，因为计算机的底层指令集可以一次性处理32位。
Common Lisp提供了一个完整的函数补充，来操作比特向量和整型数。下面的表格列出了一些，以及他们对应的列表函数。

处理列表的函数|处理整型数的函数|处理比特向量的函数
---|---|---
Intersection|logand|bit-and
Union|logior|bit-ior
Set-difference|logandc2|bit-andc2
Member|logbitp|bit
Length|logcount|

例子
`(intersection '(a b c d) '(a be)) => (A B) `
`(bit-and #*11110 #*11001) => #*11000 `
`(logand #b11110 #b11001) => 24 == #b11000`
#####3.10 具有破坏性的函数
在数学中，一个函数知识对给定的输入参数计算输出的值。函数不会真的做任何事情，仅仅是计算结果。例如，如果说x = 4，y = 5并且将加函数应用在这两个参数x和y上，期待的答案一定是9.如果说，计算之后的x的值是什么？x的值被改变的话肯定会让人大吃一斤。在数学中，将运算符应用到x不会对x本身的值有任何作用。
在Lisp中，有一些函数步进可以计算结果，也能产生一些效果。这些函数就不是数学概念上的函数了，在其他语言中被称作过程。当然，Lisp中的大部分函数是数学函数，但是也有不是的。他们这种破坏性函数在特定的情况下是很有用的。
`> (setf x '(a b c)) => (A B C) `
`> (setf y '(1 2 3)) => (1 2 3) `
`> (append x y) => (A B C 1 2 3)`
Append是一个纯粹的函数，所以在对append的调用求值之后，我们可以肯定x和y还是原来的值。
`> (nconc x y) => (A B C 1 2 3) `
`> X => (A B C 1 2 3) `
`> y ¬=> (1 2 3)`
函数nconc和appned的计算结果是相同的，但是却有一个副作用，他会更改第一个参数的值，所以他被称作是hi一耳光破坏性函数，因为他会破坏原有的结构，代之以一个新的结构。这意味着对使用这个nconc函数的程序猿会有一个概念上的负担。他必须知道第一个参数的值将会有所变化，这种考虑要比使用非破坏性函数复杂多了，因为程序员需要关心函数调用的结果和副作用。
Nconc的好处就是他不会使用任何存储空间，append必须做一个X的拷贝之后追加到y上，nconc不需要拷贝任何东西。而是只要更改x的最后一个元素的rest部分，指向y就可以了。所以当需要保留存储空间的时候就需要使用破坏性函数，但是也要意识到他的后果。
除了nconc还有很多n开头的破坏性函数，包括nreverse，nintersection，nunion，nset-difference和nsubst。很重要的一个例外就是delete，他是非破坏性函数remove的对应版本。当然，特殊形式setf也用来更改结构，但是他是最危险的非破坏性函数，因为很容易忽视他们的效果。
【h】更改结构练习。写一个程序来扮演游戏二十个问题的回答者角色。程序的用户会在脑中想像热和种类的事物。程序会问人问题，用户必须回答，是，否，猜中的时候就回答猜对了。如果程序猜测，超过了二十个问题，就会放弃猜测直接问是什么东西。一开始程序玩的很蓝，但是每一次运行，他会记忆用户的回答，并在之后的猜测中使用。
#####3.11 数据类型概览
本章是围绕函数来组织的，当类似的函数就放在一起。但是在Common Lisp的概念里还有另一种组织方式，就是通过不同的数据结构来组织。这样子的理由主要有两点：第一，他会给出一个可选的功能种类的不同方式。第二，数据类型本身就是Common Lisp语言的对象，有很多函数用来操作数据类型。还有就是主要在做出声明和测试对象（使用typecase宏）。
下面的表格是最常用的数据类型：

数据类型|样例|含义解释
---|---|---
Character 字符|#\c|单个的字符，数字或者标点符号标记
Number 数字|42|最常用的数字就是浮点数和整型数
Float 浮点数|3.14159|使用十进制小数点的数字
Integer 整型数|42|一个整数，定长的或者变长的数字
Fixnum 定长数|123|用单字长存储的整型数
Bignum 变长数|123456789|不绑定长度的整型数
Function 函数|#’sin|带一个参数列表的函数
Symbol 符号|sin|符号可以用来命名函数或者变量，或者可以指向自己的对象
Null 空|nil|对象nil是唯一的空类型对象
Keyword 关键字|:key|关键字是符号类型的子类型
Sequence 序列|(a b c)|序列包括列表和向量
List 列表|(a b c)|一个列表就是一个组合单元cons或者是空null
Vector 向量|#(a b c)|向量是序列的子类型
Cons 组合|(a b c)|组合就是非空列表
Atom 原子|t|原子就是，不是组合的话就是一个原子
String 字符串|”abc”|字符串就是字符向量的一个类型
Array 数组|#1A(a b c)|数组包含了向量和高维数组
Structure 结构|#S(type …)|结构通过defstruct来定义
Hash-table 哈希表|…|哈希表通过make-hash-table来创建

几乎每一种类型都有一个分辨器断言——就是一种判断是不是这个类型的函数。一般来说，一个断言的返回值只有两种：真或者假。在Lisp中，假的值就是nil，其他的都被认为是真值，一般来说真值的表示是t。一般来说，分辨器断言的名字是用类型名加上字母p来组成的：characterp就是用来分辨字符，numberp就是用来分辨数字，等等等等。例如，(numberp 3)的返回值是t，因为3是一个数字，但是(number “x”)就会返回nil，因为x不是一个数字是一个字符串。
Common Lisp有一个不好，就是没有给所有类型实现分辨器，定长数，变长数，序列和结构就没有分辨器断言。有两个分辨器null和atom是不以p结尾的断言。还有就是在p和之前的字符有一个连字符的，比如hash-table-p，是因为之前的名字就有连字符。另外，所有由defstruct生成的分辨器断言p之前都有一个连字符。
函数type-of返回的是它的参数的子类型，typep是用来测试一个对象是不是指定的类型，函数subtype测试的是一个类型是不是可以分成另一个子类型。例如：
`> (type-of 123) => FIXNUM `
`> (typep 123 'fixnum) => T `
`> (typep 123 'number) => T `
`> (typep 123 'integer) => T `
`> (typep 123.0 'integer) => NIL `
`> (subtypep 'fixnum 'number) => T`
在Common Lisp中的类型层次是有一些复杂。如上面的表格显示，数字就有很多不同的类型，像123就可以被看做是定长，整型或者和数字类型。之后我们还会看到类型rational分数和t。
类型层次是一个图状拓扑关系，而不仅仅是一个树。例如，向量同时是一个序列和一个数组，虽然数组和序列互相之间不是属于子类型关系。相似的null就同时是symbol和list的子类型。
下面的表格是一些不常用到的数据类型：

数据类型|样例|含义解释
---|---|---
T|42|每一个对象都是t类型
Nil||没有对象是nil类型的
Complex 复数|#C(0 1)|虚数类型
Bit 位|0|比特位，0或者1
Rational 有理数|3/2|有理数包括整数和分数
Ratio 分数|2/3|精确地分数
Simple-array 简单数组|#1A(x y)|不可以替换或者改变的数组
Readtable|…|字符和可读取含义的映射
Package|…|模块形式的符号集合
Pathname|#P”/usr/spool/mail”|文件或者目录名
Stream 流|…|一个打开的文件的指针；用来读取或者打印
Random-state |…|用来做random的种子的状态

另外还有一些更加特殊的类型，比如short-float，compiled-function，和bit-vector。也可以构造一些更加精确地类型比如，(vector (integer 0 3) 100)，意思是一个100个元素的向量，每一个元素都是从0到3的整数。第10.1章节会有关于特定类型和使用的详细介绍。
断言可以用来判断每一个数据类型，也可以用来根据一些特定的条件进行判断。例如oddp就用来判断奇数，string-greaterp用来判断一个字符串是不是在字符意义上比另一个更大。
#####3.12 输入输出I/O
Lisp中的输入是非常简单的，因为用户可以用一个完整的语法语义解析器。这个解析器叫做read。他用来读取，返回一个Lisp表达式。你也可以设计一个自己的read版本应用来解析输入。还有，read的输入不一定是要合法的可以求值的Lisp表达式才可以。就是说，你可以读取(“hello” cons zzz)，就像读取(+ 2 2)一样。有些情况Lisp表达式也不能很好地运作，比如函数read-char就用来读取单个字符，read-line就会把接下来的一行所有的内容读取，然后作为字符串返回。
从终端读取输入，函数read，read-char或read-line（没有参数的话）就会分别反悔一个表达式，一个字符和一个字符串。从文件中读取也是可以的，函数open和with-open-stream可以用来打开一个一个文件并关联到一个流上，stream就是Lisp中对于文件输入输出描述符的名字。这三个读取函数都接受三个可选的参数。第一个就是要读取的流。第二个，如果为t，会在遇到文件末尾的时候提出错误。第二个参数如果为nil，第三个参数就是设置，遇到文件末尾的时候返回的值。
Lisp中的输出类似于其他语言中的输出，例如C语言。有一些底层的函数来做一个特定种类的输出，也有一些通用的函数来做格式化输出。函数print会在一个新行上打印任何对象，后跟一个空格。Prinl不开新行，也没有后跟空格打印对象。两个函数的打印形式都是用的read可以处理的方式。Lieu字符串”hello there”就会打印成”hello there”。函数princ被用作打印人类可以阅读的形式。同样的字符串就会打印成hello there，就把双引号去掉了。那也就是说read就不能再回复原有的格式了；raed会将其解释为两个符号，而不是一个字符串。函数write接受是一个不同的关键字参数，根据不同的设置可以将行为表现的像prinl和princ函数一样。 
输出函数也接受一个流作为可选的参数。接下来，我们创建文件test.text，并且在里面打印两个表达式。之后我们会打开文件读取，尝试读回第一个表达式，一个字符和之后的两个表达式。请注意read-char函数返回的是字符井号\G，后面read读取字符OODBYE之后将他们转化成一个符号。最后raed会遇到文件结束，返回的是一个特殊值，eof。
`> (with-open-file (stream "test.text" :direction :output) `
`(print '(hello there) stream) `
`(prine 'goodbye stream) ¬ =>`
`GOODBYE ; and creates the file test.text `
`> (with-open-file (stream "test.text" :direction :input) `
`(list (read stream) (read-char stream) (read stream) `
`(read stream nil 'eof))) => `
`((HELLO THERE) #\G OODBYE EOF)`
函数terpri是中断打印行（terminate print line）的缩写，之后就跳过下一行。函数fresh-line也会挑货下一行，除非输出已经在行首的话。
Common Lisp也提供了通用的输出函数，叫做format。Format的第一个参数总是一个流，往这个流中打印，使用t的话就是打印到终端上。第二个参数就是格式化字符串，format会珠子原样输出，除非碰到了格式指令，字符~打头的一些指令。这些指令会告诉函数如何打印后面的参数。C中的printf函数和FORTRAN中的format函数和这里的格式化输出概念很相似。
`> (format t "hello, world") `
`hello,world `
`NIL`
当我们使用格式化控制指令和附加参数结合的时候，就会兰道有意思的结果：
`> (format t "~&~a plus ~s is ~f" "two" "two" 4) `
`two plus "two" is 4.0 `
`NIL`
~&的意思是新开一行，~a的意思是如princ函数打印的形式打印，~s的意思是下一个参数用prinl的形式打印，~f的意思就是浮点数格式打印数字。如果参数不是一个数字，那么就会用princ打印。Format的返回值总是nil。总共是有26个不同的格式化控制指令。下面是一些复杂的例子：
`> (let ((numbers '(1 234 5)) `
 `(format t "~&~{~r~^ plus ~} is ~@r" `
   `numbers (apply #'+ numbers))) `
`one plus two plus three plus four plus five is XV `
`NIL`
指令~r会将下一个参数，应该是数字，用英语的形式打印出来，而指令~@r会将数字用罗马数字打印出来。组合之灵~{…~}的意思是接受一个列表，根据大括号内的格式化指令输出每一个元素。最后指令~^会跳出大括号的循环中。如果说没有更多元素的话。你可以看到format就像loop一样，包含了几乎一整个编程语言，也像loop一样，不是很符合Lisp的语言风格。
#####3.13 调试工具
在很多语言中，调试的策略有两种，一种是编辑程序，擦呼吁一些打印语句，重新编译然后再次尝试。第二中就是使用调试工具，查看或者更改运行中的程序状态。
Common Lisp对两种策略都是认可的，但是也提供了第三种：给程序加上注释，注释本身并不是程序的一部分，但是会有自动更改运行中程序的效果。第三种策略的好处就是一旦完成了之后就不需要退回去更改在第一种策略中留下的更改。另外Common Lisp提供了显示程序信息的函数。不是一定只能依赖看源代码了。
之前我们已经见过，trace和untrace用来追踪程序的调试信息。另一个有用的工具是step，可以再每一个子形式求值之前中断执行。形式(step 表达式)会在求值的同时但会表达式，三十会在给定的点暂停下来，允许用户在下一步执行之前查看计算，更改一些东西。这个命令是根据Lisp实现的不同来决定有没有提供的，可以用问号来查看命令列表看有没有。下面的一个例子是我们步进查看一个表达式两次，一次是在每一次子求值之前暂停，另一次是跳转到每一次函数调用。在这个实现中，命令就是控制字符，所有他们不会在输出中出现。所有的输出，包括左右箭头，都是有步进器打印的，我没有加任何标记：
`> (step (+ 3 4 (* 5 6 (/ 7 8)))) `
`<= (+ 3 4 (* 5 6 (/ 7 8))) `
 `<=3=>3 `
 `<=4=>4 `
 `<= (* 5 6 (/ 7 8)) `
   `<=5=>5 `
   `<=6=>6 `
   `<= (/ 7 8) `
     `<=7=>7 `
     `<=8=>8 `
   `<= (/ 7 8) => 7/8`
 `<= (* 5 6 (I 7 8)) => 105/4 `
`<= (+ 3 4 (* 5 6 (I 7 8))) => 133/4 `
`133/14`
`> (step (+ 3 4 (* 5 6 (/ 7 8)))) `
`<= (+ 3 4 (* 5 6 (/ 7 8))) `
&emsp;`/: 7 8 => 7/8 `
&emsp;`*: 5 6 7/8 => 105/4 `
&emsp;`+: 3 4 105/4 => 133/4 `
`<= (+ 3 4 (* 5 6 (I 7 8))) => 133/4 `
`133/14`
函数describe，inspect，documentation，和apropos都提供了当前程序的状态信息。Propos打印的关于所有匹配参数名的符号信息。
`> (apropos 'string) `
`MAKE-STRING 	function (LENGTH &KEY INITIAL-ELEMENT) `
`PRIN1-TO-STRING   function (OBJECT) `
`PRINC-TO-STRING 	function (OBJECT)`
`STRING 			function (X)`
`…`
知道对象名字之后，describe函数就可以用来获取进一步的信息
`> (describe 'make-string) `
`Symbol MAKE-STRING is in LISP package. `
`The function definition is #<FUNCTION MAKE-STRING -42524322>: `
`NAME: MAKE-STRING `
`ARGLIST: (LENGTH &KEY INITIAL-ELEMENT) `
`DOCUMENTATION: "Creates and returns a string of LENGTH elements, `
`all set to INITIAL-ELEMENT." `
`DEFINITION: (LAMBDA (LENGTH &KEY INITIAL-ELEMENT) `
`(MAKE-ARRAY LENGTH :ELEMENT-TYPE 'CHARACTER `
`:INITIAL-ELEMENT (OR INITIAL-ELEMENT `
`#\SPACE))) `
`MAKE-STRING has property INLINE: INLINE `
`MAKE-STRING has property :SOURCE-FILE: #P"SYS:KERNEL; STRINGS" `
`> (describe 1234.56) `
`1234.56 is a single-precision floating-point number. `
`Sign 0, exponent #0211, 23-bit fraction #06450754`
如果只是想要符号的文档字符串，函数documentation会取得：
`> (documentation 'first 'function) => "Return the first element of LIST." `
`> (documentation 'pi 'variable) => "pi"`
如果你想查看或者更改复杂结构的租金啊，工具inspect可以使用。在一些实现中，他会调用一个基于窗口的浏览器。
Common Lisp也提供一个在出错的时候自动进入的调试器，错误或许是无意的或许是语义的深层错误，都会自动进入调试器。调试器的细节根据实现不同而不同，但是进入调试器的方法是有标准的。函数break会进入调试器然后打印条可选的信息。这是有意设置成为调试断点的主要方法。Break只为调试目的服务；当一个程序被认为是可以运行，所有对break的调用都应该移除。然而，使用函数error，cerror，assert，或者chek-type做一个非常条件下的检查或许是个好主意，这些函数我们在之后会介绍。
#####3.14 防错工具
在代码中包含防错检查是除了正常调试外的一个很好地做法。防错代码会检测错误并且可能会做出正确的操作。
函数error和cerror就是用来在出错的时候发出信号。这些函数调用即使是在调试之后也会保留。函数error接受一耳光格式化字符串和一些可选参数。如果发出的是致命错误信号，程序就会停止运行，并且不让用户再启动了。例如：
`(defun average (nubers)`
 `(if (null numbers)`
   `(error “Average of the ampty list is undefined.”)`
   `(/ (reduce #’+ numbers)`
     `(length numbers))))`
在很多情况下，致命错误是很猛的。还有一个函数cerror是可继续错误的缩写。Cerror接受两个格式化字符串；第一个会打印信息，显示如果我们继续会发生什么，第二个打印错误信息本身。Cerror实际上不会做任何修正错误的操作，只不过允许用户发出认可错误继续运行的信号罢了。在下面的实现中，用户通过键入:continue来继续运行。在ANSI Common Lisp中，还有其他的方式来制定继续的选项。
`(defun average (numbers) `
 `(if (null numbers) `
   `(progn `
     `(cerror "Use 0 as the average." `
       `"Average of the empty list is undefined.") `
     `¬ 0) `
     `(/ (reduce #'+ numbers) `
       `(length numbers)))) `
`> (average ' () ) `
`Error: Average of the empty list is undefined. `
`Error signaled by function AVERAGE. `
`If continued: Use 0 as the average. `
`>> :continue `
`0`
在这个例子中如果加入错误检查的话，会让代码的长度倍增。一般不会这么做，这也是工作在给定输入的代码和工作在所有错误环境下的代码的一个重大区别。Common Lisp尝试使用一个实现一些特殊形式来提供错误检查。Ecase形式就是error case的缩写，他就像一个正常的case形式，除了一点，要是没有情况满足条件，就会报错。还有ccase是continuable case的缩写。和ecase一样，除了错误是可继续的。系统要求测试对象有一个新的值，知道说用户支持了匹配的对象之一。
为了让错误检查不会带来代码规模的膨胀，Common Lisp提供了特殊形式check-type和assert。如其名，check-type用来检查参数类型。如果参数的类型错误，就会报出一个可继续错误。例如：
`(defun sqr (x)`
 `“Multiply x by itself.”`
 ` (check-type x number)`
 ` (* x x))`
如果sqr的参数不是一个数字的话就对报出一个适当的错误信息：
`> (sqr "hello") `
`Error: the argument X was "hello", which is not a NUMBER. `
`If continued: replace X with new value `
`>> :continue 4 `
`16`
Assert比check-type更加通用，在最简单的形式中，assert测试一个表达式，根绝返回值的真假来报出信号：
`(defun sqr (x) `
 `"Multiply x by itself." `
 ` (assert (numberp x)) `
 ` (* x x))`
出现这种断言想再继续是不可能的了。但是可以给assert一系列的可修改的参数，尝试使得assert的返回值为真。下面的例子中，变量x就是唯一一个可以改变的：
`(defun sqr (x) `
 `"Multiply x by itself." `
 ` (assert (numberp x) (x)) `
  `(* x x))`
如果违法了这个断言，就会打印出一个错误信息，用户会被给与一个可继续的选项来更改x。如果x的值满足了断言，程序就会继续，assert的返回值永远是nil。
最后，对于想要更多的丛植错误信息的用户可以提供一个格式控制字符串和可选选项。所以最复杂的assert语法是：
(assert测试形式部分 (位置…) 格式化控制字符串 格式化参数…)
这里是另一个例子。程序执行前断言检测，熊喝的麦片粥是不是太烫了还是太冷了。
(defun eat-porridge (bear)
  (assert (< too-cold (temperature (bear-porridge bear)) too-hot)
    (bear (bear-porridge bear))
    “~a’s porridge is not just right:~a”
   Bear (hotness (bear-porridge bear)))
 (eat (bear-porridge bear)))
在下面的交互过程中，断言失败了，打印了程序的错误信息，还有两个可以继续的可能选项。用户选择一个，调用make-porridge输入一个新的值，函数就成功继续了。
`> (eat-porridge momma-bear) `
`Error: #<MOMMA BEAR>'s porridge is not just right: 39 `
`Restart actions (select using :continue): `
`0: Supply a new value for BEAR `
`1: Supply a new value for (BEAR-PORRIDGE BEAR) `
`>> :continue 1 `
`Form to evaluate and use to replace (BEAR-PORRIDGE BEAR): `
`(make-porridge :temperature just-right) `
`Nil`
如果程序运行OK的话，好像也不必要浪费时间来写断言。但是对于很多不是全知全能的程序员，bug总是层出不穷，花在排错上的时间还不如写断言来的省时省力。
无论何时，拟开发了一个复杂的数据结构，比如某种数据库，开发一个对应的一致性检查器总是一个好的做法。一致性检查就是看看整个数据结构测试所有可能的错误。当发现了一个新的错误，这个错误的检查就应该成为一致性检查的一部分了。调用一致性检查是最快的帮助定位数据结构中bug的方式。
另外，保持进行一些测试也是很好的作法，当程序更改之后，很容易将之前排出的bug重新引回程序中。这种发福测试叫做回归测试，Waters在1991年诈尸了一个维护回归测试的工具集。但是也是很简单的，可以使用一系列assert调用来委会一个非常规测试。
(defun test-ex () 
"Test the program EX on a series of examples." 
(init-ex) ; Initialize the EX program first. 
(assert (equal (ex 3 4) 5)) 
(assert (equal (ex 5 0) 0)) 
(assert (equal (ex 'x 0) 0)))
######时间测试工具
一个完整的程序不仅仅可以给出正确的输出，还要考虑程序的效率。(time 表达式)可以用来看看执行这个表达式用了多少时间，一些实现也会打印静态的内存占用空间：
`> (defun f (n) (dotimes (i n) nil)) => F`
`> (time (f 10000)) => NIL `
`Evaluation of (F 10000) took 4.347272 Seconds of elapsed time, `
`including 0.0 seconds of paging time for 0 faults, Consed 27 words. `
`> (compile 'f) => F `
`> (time (f 10000)) => NIL `
`Evaluation of (F 10000) took 0.011518 Seconds of elapsed time, `
`including 0.0 seconds of paging time for 0 faults, Consed 0 words.`
信息显示，编译版本的程序快了300倍，而且用的空间更少。大部分严谨的Common Lisp程序员都会使用编译版本的程序工作。然而一般看来，在开发程序的伊始就考虑效率问题是不大合适的。更好的是设计一个灵活性比较高的程序，先运行起来，之后在修改的更加高效。换句话说，就是讲开发阶段和优化阶段分开。第9章和第10章会给出提高效率的更多细节，第25章会给出更多关于调试和排错技术的建议。
#####3.15 求值
Lisp中有三个函数是做求值的：funcall，apply和eval。Funcall用于吧一个函数应用在独立的函数上，apply是用于将一个函数应用到参数的列表上。实际上apply可以再最后的参数之前有一个或者多个独立的具有完整形式的参数，函数或者特殊形式，之后又参数或者原子。下面的五个语句是相等的：
`> (+ 1 2 3 4) => 10 `
`> (funcall #'+ 1 2 3 4) => 10 `
`> (apply #'+ '(1 2 3 4)) => 10 `
`> (apply #'+ 1 2 '(3 4)) => 10 `
`> (eval '(+ 1 234)) => 10`
过去的观念是，eval就是Lisp灵活性的关键所在。在现代Lisp版本中，使用静态域的版本，比如Common Lisp，eval的使用越来越少（事实上，Scheme中就没有eval）。代之的是，程序员虎使用lambda表达式创建一个新函数，之后用apply或者funcall来调用函数。一般来说，如果你发现你正在使用eval，那一般就是你做错了。
#####3.16 闭包
创建一个新函数的意思到底是什么？当然每一次一个特殊形式function或者井号单引号的简略形式被求值的话，一个函数就会被反悔。但是在这个例子中我们看到，返回的函数总是相同的。
`> (mapcar #'(lambda (x) (+ x x)) '(1 3 10)) => (2 6 20)`
每一次我们对lambda表达式求值，返回的函数就会对后面的参数进行加倍操作。然而在一般情况下，一个函数是由函数的主体和函数像伴随的函数引用自由词域变量组成的。这样的一个组合叫做词法闭包，或者简称闭包，因为语法变量是闭包在函数中的。看下面的例子：
`(defun adder (c) `
 `"Return a function that adds c to its argument." `
 `#' (l ambda (x) (+ xc))) `
`> (mapcar (adder 3) '(1 3 10)) => (4 6 13) `
`> (mapcar (adder 10) '(1 3 10)) => (11 13 20)`
每一次我们给c不同的值来调用adder，他都会创建一个不同的函数，函数会把c加到它的参数上。既然每一次对adder的调用都创建了一个不同的本地变量c，那么每一次adder返回的函数也会是一个不一样的函数。下面是另一个例子，函数bank-account返回一个可以用来作为银行账号形式的闭包。这个闭包取得本地变量balance。必报的主体提供代码来访问和修改本地变量。
`(defun bak-account (balance)`
 `“Open a bank account starting with the given balance.”`
 `#’(lambda (action amount)`
   `(case action`
     `(deposit (setf balance (+ balance amount)))`
     `(withdraw (setf balance (- balance amount))))))`
下面的凉席对于bank-account 的调用创建了两个不同的闭包，每一个词法变量balance都有不同的值。随后对两个闭包的调用分别改变了它们的变量的值，但是两个账户之间是没有混淆的。
`> (setf my-account (bank-account 500.00)) => #<CLOSURE 52330407>`
`> (setf your-account (bank-account 250.00)) => #<CLOSURE 52331203> `
`> (funcall my-account 'withdraw 75.00) => 425.0 `
`> (funcall your-account 'deposit 250.00) => 500.0 `
`> (funcall your-account 'withdraw 100.00) => 400.0 `
`> (funcall my-account 'withdraw 25.00) => 400.0`
这种编程风格会在第13章有更加详细的介绍。
#####3.17 特殊变量
Common Lisp提供了两种变量：词法变量和特殊变量。对初学者来说，会把这个概念和其他语言总的全局变量进行等同。但是这样会导致一些问题。最好是将Common Lisp中的术语进行独立的理解。
Common Lisp默认的变量都是词法变量。词法变量的引入是通过一些系那个let或者defun之类的语法结构来引入的，而且他们的名字能被引用的范围，在代码中也是有限的。这个范围被称作变量的作用域。
变量作用域这个概念上，Common Lisp和其他语言是没有区别的，可以叫做变量的范围，或者生命周期。在其他语言中，范围是等同于作用域的：在进入一块代码的时候创建的本地变量，离开块代码的时候就会销毁。但是因为Lisp中可以创建新的函数，闭包，因此代码中引用的变量的生存周期可以在离开代码作用域之后任然继续存在。再来看看bank-acount函数，他创建了一个表现为银行账号的闭包：
`(defun bank-account (balance) `
 `"Open a bank account starting with the given balance." `
 `#'(lambda (action amount) `
   `(case action `
     `(deposit (setf balance (+ balance amount))) `
     `(withdraw (setf balance (- balance amount))))))`
函数引入了一个词法变量balance。Balance的作用域就是函数的主体，因此对于balance的引用只能在作用域中进行。那bank-account被调用和结束的时候发生了什么呢？作用域是结束了，但是balance的范围还在延续。我们可以调用闭包，闭包可以引用变量balance，因为这个创建闭包的代码是在balance的作用域中的。
总的来说，Common Lisp词法变量是不一样的因为他们可以获取闭包，之后甚至在控制流之后指向他们的作用域。
现在我们来看看特殊变量，一个变量被称作特殊，是要通过defvar或者defparamerter来定义。例如：
`(defvar *counter* 0)`
这样就可以在程序的任何地方指向变量counter。这时类似于全局变量，不一样的部分是，全局绑定可以通过本地绑定来进行屏蔽。在大部分语言中，本地绑定会引入一个本地词法变量，但是在Common Lisp中，特殊变量可以同时进行本地的和全局的绑定。下面是例子：
`(defun report () `
 `(format t "Counter = -d " *counter*))`
`> (report) `
`Counter = 0`
`NIL `
` > (let (*counter* 100)) `
`(report)) `
`Counter = 100 `
`NIL `
` > (report) `
`Counter = 0 `
`NIL`
这里对report有了三次调用。第一次和第三次，report都打印的是特殊变量counter的全局值。第二个调用，let形式引入了一个对特殊变量counter的新的绑定，之后在打印的值。一旦let的作用域结束，新的绑定就被销毁，所以report就重新开始使用全局值。
总的来说，Common Lisp特殊变量的不同是因为他们有全局作用域，但是也承认本地的（动态）屏蔽的可行。请记住：一个词法变量有词法作用域和不确定的范围。夜歌特殊变量有不确定的作用域和动态的范围。
函数调用(symbol-value var)，这里var是求值成一个符号，可以用来获得一个特殊变量的当前值。为了设置一个特殊变量，下面两种形式是完全等同的：
`(setf (symbol-value var) value) `
`(set var value)`
Var和value会被求值。没有访问和设置词法变量的对应形式。特殊变量在符号和值之间设置一个映射来访问运行中的程序。这不像词法变量（或者所有传统语言中的变量），符号（标识符）只在程序被编译的时候有意义。一旦程序运行，标示符就会被编译，也就不能再访问变量。之后出现在一个词法变量的作用域中的代码可以引用变量。
【s】给定下面词法变量a和特殊变量b的初始化，let形式的值是什么？
`(setf a 'global-a) `
`(defvar *b* 'global-b) `
`(defun fn () *b*) `
`(let (( a 'local-a) `
`(*b* 'local-b) ) `
`(list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))`
#####3.18 多值
本书通篇都在讲，函数返回的值。历史上，Lisp设计就是每一个函数都会返回值，即使是那些看上去更像过程而不是函数的函数也会返回一个值。但是有时候我们想要函数返回不止一个信息。当然，我们可以通过列表或者结构来存储信息，但是我们就要面临定义结构，每一次都要构建实例，还有解析实例等等麻烦的事情。看看函数round，它是一种可以将一个浮点数四舍五入成整数的函数。所以(round 5.1)的结果就是5。有时候，程序员会需要小数部分。函数round就可以返回两个值，整数部分和小数部分：
`> (round 5.1) => 5 .1`
箭头后面有两个值是因为round就返回两个值。大部分时候多个值是会被忽视的，仅仅使用第一个值。所以(* 2 (round 5.1))的结果就是10，就像round只是一个返回单值的函数一样，如果你想要获得多值，你就一定要使用特殊形式，multiple-value-bind：
(defun show-both (x) 
 (multiple-value-bind (int rem) 
    (round x) 
  (format t "-f = -d + -f" x int rem))) 
`> (show-both 5.1) `
`5.1 = 5 + 0.1`
你可以使用函数values来定义你自己的多值函数，values会将他的参数返回成多个值：
`> (values 1 2 3) => 1 2 3`
多值是个不错的方案，因为不需要的话他就不引人注目。大部分时候使用round都是需要他的单值的整数部分。如果round不适用多值，如果将两个值打包成一个列表或者结构，之后会在一般情况下很难使用。
当然不返回值也是可以的,比如(values)表达式，有些过程就是为了他的效果而设计，比如打印函数。例如，describe及定义成打印信息之后不返回值。
`> (describe 'x) ``
`Symbol X is in the USER package. `
`It has no value, definition or properties.`
但是当任何不返回值的表达式是嵌套在一个会返回值的上下文里面，仍然遵守Lisp一个表达式一个返回值的规则，返回的是nil。在下面的例子，describe不返回值，但是之后list会要求一个值，获取的是nil。
`> (list (describe 'x)) `
`Symbol X is in AILP package. `
`It has no value, definition or properties. `
`(NIL)`
#####3.19 关于参数
Common Lisp给用户在定义函数形式参数上赋予了很多灵活性，也就延伸到了函数接受的实际参数上。下面的程序是一个算数练习。他会问用户一系列的n问题，没一个问题都是测试算术运算符op。运算符的实际参数将会是随机数：
(defun math-quiz (op range n) 
 "Ask the user a series of math problems." 
  (dotimes (i n) 
    (problem (random range) op (random range)))) 
 (defun problem (x op y) 
 "Ask a math problem, read a reply, and say if it is correct." 
  (format t "-&How much is -d -a -d?" x op y) 
  (if (eql (read) (funcall op x y» 
    (princ "Correct!") 
    (princ "Sorry, that's not right.")))
下面是运行的例子：
`> (math-quiz '+ 100 2)` 
`How much is 32 + 60? 92 `
`Correct! `
`How much is 91 + 19? 100 `
`Sorry, that's not right.`
这个math-quiz函数的问题在于他要求用户键入三个参数：运算符，范围和迭代的次数。用户必须记住参数的顺序，还要记住引用的运算符。而且这些都预先设定了用户是会加法的！
Common Lisp提供了两种方式来处理这个问题。第一，程序员可以定义参数是可选的，并且为这些参数设定默认值。例如，在math-quiz函数中我们可以安排+运算作为默认的运算符，100是默认的数字范围，还有10是默认的迭代次数。
(defun math-quiz (&optional (op '+) (range 100) (n 10)) 
 "Ask the user a series of math problems." 
  (dotimes (i n) 
    (problem (random range) op (random range)))
现在，(math-quiz)的意思和(math-quiz ‘+ 100 10)是一样的。如果一个可选的参数单独出现而没有默认值设定的话，默认值就是nil。可选参数是很方便的；然而，假如用户想要设置这些参数怎么办？都OK，只要显示的输入对应位置就可以了(math-quiz ‘+ 100 5)
Common Lisp也允许参数是位置不想管的。这些关键字参数就是在函数调用中显式命名的。当有很多默认参数，都有了默认值，想要设定特定的参数的值的是后就很有用了。我们可以这样定义math-quiz：
(defun math-quiz (&key (op '+) (range 100) (n 10)) 
 "Ask the user a series of math problems." 
  (dotimes (i n) 
    (problem (random range) op (random range)))
现在来说 (math-quiz :n 5)和(math-quiz :op '+:n 5 :range 100)的意思就是一样的了。关键字参数可以用参数的名字来定义，之前加上一个冒号，后面就是设定的值，这种关键字，值的对可以安排任何顺序。
不仅仅用在参数列表中，冒号开头的符号称作关键字可以用在任何地方。Lisp中使用的术语关键字和IQ他语言中的关键字概念是不一样的。例如，Pascal中的关键字（或者叫保留字）就是指愈发符号，if，else，begin，end等等。在Lisp中我们成这样的符号叫做特殊形式操作符或者简称特殊形式。Lisp关键字就是存在在关键字包中的符号。包就是一张符号表，是字符串和命名符号之间的映射。这些关键字没有特殊的意义，虽然他们是有一些不寻常的特性：关键字是常量，求值为自身，不像其他的符号，求值的结果是以符号命名的变量。关键字也刚好被用来定义&key参数列表，但是这是对他们的值的好处，不是对语法规则的效果。很重要的事情是关键字可以用在函数调用，但是一般的非关键字符号会用在函数定义的参数上。
由于历史原因，令人困惑的一点就是符号&optional，&rest和&key被称作lambda列表关键字。不像带冒号的关键字，这种在lambda列表关键字中的&符号没有特殊的含义，看一下这些标记样例：
`> :xyz =? :XYZ  ; keywords are self-evaluating `
`> &optional => ¬ ; lambda-list keywords are normal symbols`
`Error: the symbol &optional has no value `
`> '&optional => &OPTIONAL`
`> (defun f (&xyz) (+ &xyz &xyz)) => F ;&hasnosignificance`
`> (f 3) => 6 `
` > (defun f (:xyz) (+ :xyz :xyz)) =>`
`Error: the keyword :xyz appears in a variable list. `
`Keywords are constants, and so cannot be used as names of variables. `
`¬ > (defun 9 (&key x y) (list x y)) => G `
`¬ > (let (( keys '(: x : y : z) ) ) ; keyword args can be computed `
`(g (second keys) 1 (first keys) 2)) => (2 1)`
本章出现的很多函数都可以接受关键字参数来是的函数功能更加强大。例如，回忆一下find函数，就可以用一个特定的元素来搜索序列。
`> (find 3 '(I 2 3 4 -5 6.0)) => 3`
实际上find是接受可选的关键字参数的：
`> (find 6 '(I 2 3 4 -5 6.0)) => nil`
之所以搜索不到是因为find使用的相等测试是eql，这样子6和6.0是不相等的。然而，在equalp中6和6.0是相等的，所以我们使用test关键字：
`> (find 6 '(I 2 3 4 -5 6.0) :test #'equalp) => 6.0`
我们可以对test关键字使用任何二进制断言，例如，在序列中找找第一个大于4的数字：
`> (find 4 '(I 2 3 4 -5 6.0) :test #'<) => 6.0`
假设我们现在是不关心数字前的正负号；如果我们搜索5，我们想找到-5.我们可以用key关键字来接受绝对值函数来操作每一个元素：
`> (find 5 '(1 234 -5 6.0) :key #'abs) => -5`
关键字参数极大地扩展了内建函数的可用性，斌企鹅他们可以对你自己定义的函数实现一样的功能。在内建函数中，最常用的关键字一般分为两类：test，test-not和key，是用在匹配函数上，还有就是start，end和from-end是用在序列函数中的。一些函数接受关键字的集合。（CLTL不鼓励使用test-not关键字，虽然这个关键字还是语言的一部分）。
匹配函数包括了sublis, position, subst, union, intersection, set-difference, remove, remove-if, subsetp, assoc, find, 还有member。他们默认的相等测试都是eql。这个选项可以使用关键字参数test来更改，或者反过来用test-not定义。另外，比较的过程可以和对象的部分进行，不必和整个对象进行，只要用选择器函数用key参数定义就可以。
序列函数包括了remove，remove-if，position和find。最常用的序列类型就是列表，但是字符串和向量也可以看做是序列。一个序列函数会对序列中的元素反腐之星一些操作。默认的顺序就是从序列的头遍历到尾，也可以设置反过来的顺序，要使用from-end关键字，参数是t，也可以定义一个子序列，只要使用关键字start和end来接受数字就可以。序列的第一个元素的索引是0，不是1，所以要小心一些。
举个例子，结社我们要写一个类似于find或者find-if函数的序列函数，只是他返回的是所有匹配元素的列表而不仅仅是匹配的第一个元素。我们叫新的函数是find-all和find-all-if。另一种方式是，将函数看做是remove函数的变种，将所有匹配的元素留下，不匹配的元素移除。从这个观点看，我们可以看到函数find-all-if的功能实际上是和remove-if-not一样的。有时候同一个函数有两个名字是很有用的（比如not和null）。新的名字可以用defun来定义，但是拷贝定义是个更加方便的做法：
`(setf (symbol-function 'find-all-if) #'remove-if-not)`
遗憾的是，没有一个内建函数和find-all函数严格对应，所以我们不得不亲手定义一个。还好，remove可以完成很多工作。我们要做的就是安排跳过满足test的断言的元素。例如，检索列表中所有等于1的元素就是和移除不等于1的元素师一样的。
`> (setf nums '(1 2 3 2 1)) => (1 2 3 2 1) `
`¬ > (find-all 1 nums :test #'=) == (remove 1 nums :test #'/=) => (1 1)`
现在我们需要的是一个高等级的函数，可以返回一个函数的补集部分，换句话说，给定等于，我们想要不等于的部分，这样的函数在ANSI Common Lisp中称作complement，但是在早期的版本中没有定义，所以在这里给出：
`(defun complement (fn) `
`"If FN returns y, then (complement FN) returns (not y)." `
`;; This function is built-in in ANSI Common Lisp, `
`;; but is defined here for those with non-ANSI compilers. `
`#'(lambda (&rest args) (not (apply fn args))))`
当使用给定的test断言来调用find-all的时候，我们做的就是小勇remove来移除test断言的补集部分。这个方法在没有test参数部分也是成立的，因为默认就是eql。还有一个需要测试的情况就是制定test-not断言的时候，它的参数是反过来的。当test和test-not制定了同一个参数的时候，就会报错，所以我们不需要测试这个情况：
`(defun find-all (item sequence &rest keyword-args `
&emsp; &emsp; &emsp; &emsp;`&key (test #'eql) test-not &allow-other-keys) `
&emsp;`"Find all those elements of sequence that match item, `
&emsp;`according to the keywords. Doesn't alter sequence." `
&emsp;`(if test-not `
&emsp; &emsp;`(apply #'remove item sequence `
&emsp; &emsp; &emsp;`:test-not (complement test-not) keyword-args) `
&emsp; &emsp;`(apply #'remove item sequence `
&emsp; &emsp; &emsp;`:test (complement test) keyword-args)))`
这个定义唯一的难点就是理解参数列表。&rest会收集在变量关键字参数中所有的键值对。除了rest参数，还有两个特定的关键字参数，test和test-not。任何时候在参数列表中有key关键字的话，就需要一个allow-other-keys，如果其他参数允许的话。在这个情况下我们想要接受关键字，start和key然后传递参数给remove。
所有的键值对都会累加紧关键字参数的列表中，包括test或者test-not的值。所以我们就有了：
(find-all 1 nums :test #'= :key #'abs) 
== (remove 1 nums :test (complement #'=) :test #'= :key #'abs) 
(1 1)
请注意对于remove的调用包含了两个test关键字，这不是个错误，Common Lisp声明最左边的值是计数的那个。
【s】你认为为什么是两个之中最左边的那个起作用而不是右边那个？
#####3.20 Lisp还有的其他部分
Common Lisp的内容比我们在这一章看到的多的多，但是这个概览对于应付下面的章节应该是足够了，严谨的Lisp程序员应该认真的学习参考书籍或者是在线文档。在第五部分或许你会发现很有用，特别是第24章，介绍了一些Common Lisp的高级特性（比如包和错误处理）还有第25章，是对有疑惑的Lisp程序员的一个问题解决参考。
再继续介绍函数的细节的话会分散本书的主体，打断AI程序的描述不是本书的本意，毕竟AI编程才是本书的主题。
（第一部分 完）
