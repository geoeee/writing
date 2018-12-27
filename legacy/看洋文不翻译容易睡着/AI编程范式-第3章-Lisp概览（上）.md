#####第3章 Lisp概览
毫无疑问，Common Lisp是一门庞大的语言。——Guy L. Steele（另一本Lisp书籍的作者）
本章简要地概括了Lisp中最重要的特殊形式和函数。有经验的Common Lisp程序员可以随便看看或者跳过，但是对于初学者或者初次接触Common Lisp这个方言的读者是一定要认真研读的。
本章可以作为一个参考手册使用，更加权威的参考手册是CLTL第二版。CLTL的长度是本书的25倍，很明显我们在这里只能提点一下要领。更多的细节之后会在书中的程序里说明。
#####3.1 Lisp风格指南
一开始Common Lisp程序员经常被语言提供的众多选择吓到。在本章我们展示了十四个不同的方式来求出列表的长度。那怎么从中选择一个？答案之一就是参考优秀的程序，并且模仿他们的风格。一般来说，每一个程序员都应该谨记留个准则：
要具体明确一些
使用抽象机制
要简明精炼一些
用已提供的工具
不要隐匿晦涩
前后一致
使用尽可能具体的形式可以让你的代码的读者明白了解你的意图，例如条件式中when就比if更清晰。读者一看到when就会想到，接下来的测试部分是不是为真。见到if就会想起两个部分then和else。If只是用一个部分也是可以的，倾向于使用when的原因是when更加清晰。
使得具体的重要方式就是使用抽象。Lisp提供的数据结构很朴实，比如列表和数组。他们可以用来实现你需要的数据结构，但是你不应该直接调用原始函数，如果你这样定义给一个名字列表：
`(defvar *names* '((Robert E. Lee) . ..))`
你就应该定义获得这个列表每一个组件的函数。获得Lee，应该使用(last-name (first names))，而不是(caddar names)。
上面的几个准则之间，常常是互相融合的。比如说，你的代码是在列表中寻找一个元素，你应该使用find（或者find-if），而不是loop或者do。Find比一般的指令loop或者do更加明晰，这就是一种抽象，更加精巧，find是一个内建工具，很容易理解。
有时候原则之间是互相冲突的，经验会启示你应该倾向于哪一个原则比较好。看看接下来的，将一对新的键值放进一个联合列表（联合列表会在3.6节介绍）中的两种方式：
`(push (cons key val) a-list)`
`(setf a-list (acons key val a-list))`
第一种方法显得更加精简。但是第二种更加明晰，他用的acons函数就是专门为联合列表所设计的。这时候选择就变得很纠结，熟悉acons函数的可能倾向于第二种，而喜欢简洁代码的人则会用第一种。
相似的纠结会在给变量赋值的时候出现。一些人喜欢用(setq x val)，因为这样最明晰具体；其他人会用(setf x val)，这样子可以保持前后的一致，使用setf来进行所有的更新操作。无论你选择哪一种，请记住第六条原则：前后一致。
#####3.2 特殊形式
第一章说过，书中所说的特殊形式，既是指Common Lisp的语法结构，也是说这些结构中的保留关键字。使用的最普遍的关键字是：

定义用|条件式|变量|循环迭代|其他
---|---|---|---|---
defun|and|let|do|declare
defstruct|case|let*|do*|function
defvar|cond|pop|dolist|progn
defparameter|if|push|dotimes|quote
defconstant|or|setf|loop|return
defmacro|unless|incf||trace
labels|when|decf||untraced

其实只有declare，function，if，labels，let，let星，progn和quote是真正的特殊形式。其他的都是用宏的方式调用原始函数和特殊形式定义的。对于程序员来说，这没有区别，Common Lisp实现是自由转换宏和特殊形式的实现的，反之亦然，所以出于简单一直，我们会继续使用特殊形式这一个术语来统一称呼真正的特殊形式和内建宏。
######用于定义的特殊形式
本节我们研究用于定义的特殊形式，会被用在定义全局函数，宏，变量和结构。之前已经用过defun了；defmarco和defun相似，在后面介绍。
(defun 函数名 (参数。。。) “可选的文档字符串” 主体)
(defmacro 宏名 (参数。。。) “可选的文档字符串” 主体)
引入特殊变量的形式有三个。Defvar定义一个特殊变量并且可以支持赋予一个初始值和一个文档字符串。初始值的求值和赋值仅仅是在变量没有任何值的时候。Defparameter是相似的，除了它的值是必须要有的，也可以用在更改任何已存在的变量值。Defconstant被用在声明一个符号，来代表一个特定的值。
（defvar 变量名 初始值 “可选的文档字符串”）
（defparameter 变量名 初始值 “可选的文档字符串”）
（defconstant 变量名 初始值 “可选的文档字符串”）
所有def开头的形式定义的是全局的对象。定义本地变量可以使用let，定义本地函数会使用labels，之后我们再看。
大部分的编程语言提供一个组织相关对象带一个结构中的方式。Common Lisp也不例外。特殊形式defstruct就是定义一个结构类型（Pascal中叫做record类型），并且会自动定义取得结构组件的函数，一般的语法是这样：
（defstruct 结构名 “可选的文档字符串” 位置）
我们举个例子，为名字定义一个结构：
`(defstruct name`
`First`
`(middle nil)`
`last)`
自动定义的结构器函数叫做make-name，判定断言（就是判断是不是这个类型）叫做name-p，用于访问的函数佳作name-first，name-middle和name-last。（middle nil）意思是每一个由make-name创建的新名字会有一个默认的中间名nil。接下来我们来创建，访问和修改一个结构：
`> (setf b (make-name :first 'Barney :last 'Rubble)) => `
`#S(NAME :FIRST BARNEY : LAST RUBBLE) `
`¬> (name-first b) ¬=> BARNEY `
`> (name-middle b) => NIL `
`> (name-last b) => RUBBLE `
`> (name-p b) => T `
`> (name-p 'Barney) =>¬ NIL ;only the results of make-name are names `
`¬> (setf (name-middle b) '0) => 0 `
`¬> b => #S(NAME :FIRST BARNEY :MIDDLE 0 :LAST RUBBLE)`
一个结构的打印形式是用一个符号井号S开始的，之后就是结构类型和可选的键值对组成的列表。别被这个形式给迷惑了：这是一个结构的打印形式，但不是说内部结构的表现也是这样，结构的实现实际上更像向量。对于结构name，这个类型将会是零元素的想想，第一个元素是名字，第二个元素是中间名，第三个是姓。这意味着结构比列表更加高效：节省了很多空间，任何元素都可以用简单的步骤进行访问。在一个列表中，访问第n个元素需要n个步骤。
还有一些结构会有更多的控制手段，并有独立的键值位置。等出现的时候我们再介绍。
######用于条件式的特殊形式
我们用的这类特殊形式有if，就是选择的分支结构。在Lisp中，只有nil会在if的测试部分被当做false来看，其他的都是真。但传统上的惯例是将t作为正统的真值表示。
很少有特殊形式是做条件求值的，从技术阿的角度上说，if是定义成特殊形式的，其他的条件式去却都是用宏实现的，某种程度上说if是最最基础的。有些程序猿倾向于用if，也有的人倾向于最长最丰富的功能而用cond。最终的选择跟自然语言中差不多，随你高兴，自由地使用when，unless，if或者其他的，随便选一个。
下面的表是告诉我们，实际上任何一种条件式都可以用if和cond来实现。实际上这些实现是不准确的，因为or，case和cond不会对任何表达式求值超过一次，但是使用if的实现对一些表达式进行了多次求值。这张表也有对cond的实现。Cond的语法是一系列的条件语句，每一个都是由一个测试表达式和一定数量的结果表达式组成的。
(cond (测试表达式 结果表达式…)
 (测试表达式 结果表达式…)
 …)
Cond会依次读取条件从句，对每一个测试表达式求值。如果测试表达式的结果为真，那么就会对每一个结果表达式求值，返回最后一个表达式的值作为cond的值。如果一个条件从句只有测试表达式没有结果表达式，那么就会返回测试表达式本身。如果所有的测试表达式都是假，那么就会返回nil作为cond的返回值。一般来说，cond的最后一个条件从句会设置成(t 结果表达式)，这样的形式就确保肯定会有一个结果值被返回。
When和unless的形式就像cond的一个单独的条件从句，如果测试部分满足就进行求值，后面跟的是任意数量的语句序列。
And测试的是每一个语句都是不是为真，or测试的是是不是有一个为真。求值顺序都是从左向右的，只要是有条件满足，就会停止求值。也就是说，and求出有一个为假，or求出一个真，就会停止求值了。

条件式|if形式|cond形式
---|---|---
(when test a b c)|(if test (prong a b c))|(cond (test a b c))
(unless test x y)|(if (not test) (prong x y))|(cond ((not test) x y))
(and a b c)|(if a (if b c))|(cond (a (cond (b c))))
(or a b c )|(if a a (if b b c))|(cond (a) (b) (c))
(case a (b c) (t x))|(if (eql a ‘b) c x)|(cond ((eql a ‘b) c) (t x))

And和or除了在测试逻辑条件式之外的应用都是不合适的。When，unless和if都可以用来做条件判断。
(and (> n 100) 
 (princ "N is large.")) ;Bad style! 
(or <= n 100) 
  (princ "N is large.")); Even worse style! 
(cond ((> n 100) ;OK, but not MY preference 
  (princ "N is large.")) 
¬(when (> n 100) 
  (princ "N is large.")) ; Good style.
如果主要目的是返回一个值，而不是做什么操作的话，cond和if会比when和unless更加实用。When和unless是隐式返回nil，当只有一种可能性存在的话，when和unless就比if要好，if有两种分支，cond则是由多种分支。
`(defun tax-bracket (income)`
 `“Determine what percent tax should be paid for this income.”`
 `(cond ((< income 10000.00) 0.00)`
   `((< income 30000.00) 0.20)`
   `((< income 50000.00) 0.25)`
   `((< income 70000.00) 0.30)`
    `(t 0.35)))`
如果测试部分是要和一个常量比较的话，case会比较合适。
(case 表达式
(匹配 结果…) …)
对表达式求值过后会和接下来每一个匹配进行比较。只要有一个是eql相等的，结果表达式就会被求值，最后一个值就会返回。请注意匹配是不求值的。如果一个匹配表达是一个列表的话，case就会把表达式的值和列表中的每一个元素进行比较，看看是不是eql相等。如果匹配部分是一个符号otherwise（或者是符号t），就会默认执行。也就是说otherwise会放在最后一个匹配位置。
还有另一个特殊形式，typecase，会将表达式的类型和几种分支进行比较，和case类似，会选择第一个匹配条件从句进行计算。另外，特殊形式ecase和etypecase也是和case，typecase差不多的，只不过在没有匹配存在的情况下会发出错误信号。你可以吧e看成是exhaustive或者是error。Ccase和ctypecase也会发出错误，但是是可持续的错误（也就是致命错误）：
(case x
 (1 10)
  (2 20))
(cond 
  ((eql x 1) 10)
  ((eql x 2) 20))
(typecase x
  (number (abs x))
  (list (length x)))
(cond 
  ((typep x ‘number) (abs x))
  ((typep x ‘list) (length x)))
(ecase x
  (1 10)
  (2 20))
(cond 
  ((eql x 1) 10)
  ((eql x 2) 20)
  (t (error “no valid case”))
(etypecase x
  (number (abs x))
  (list (length x)))
(cond 
  ((typep x ‘number) (abs x))
  ((typep x ‘list) (length x))
  (t (error “no valid typecase”)))
######处理变量和位置的特殊形式
Setf一般是用在给变量或者位置赋予一个新的值，在其他语言中一般是用=或者:=来做这个功能。一个位置，或者说普通变量就是一个值存储的位置的名字。下面的这张表是Lisp的赋值语句和对应的Pascal形式：

;;Lisp|Pascal
---|---
(setf x 0)|x:=0;
(setf (aref A i j) 0)|A[i,j] := 0;
(setf (rest list) nil)|list^.rest := nil;
(setf (name-middle b) ‘Q)|b^.middle := “Q”;

Setf可以用来设置变量，设置结构中的组件。像Pascal语言中，出现在赋值语句左半边的表达式是被语言的语法做了严格限定的。在Lisp中，用户可以使用特殊形式defsetf或者define-setf-method来扩展setf形式。这个会在之后特别介绍。
也有一些内建的函数来更改位置。例如，(rplacd list nil)就和表达式(setf (rest list) nil)的效果一样，除了返回值不一样之外。Common Lisp的程序员大多倾向于使用setf形式而不是具体定义的函数。
如果仅仅是设置变量，特殊形式setq也可以使用。本书中我们为了前后一致会一直使用setf。
本节讨论的事情看上去是说变量（还有结构中的键值）被赋予新的值的事情。实际上很多Lisp程序是不会有赋值操作的。使用函数式编程的话，经常会引入新的变量，斗牛士一旦创建，基本上就不会再改变了。引入新变量的一个方式是作为函数的参数。也可以使用特殊形式let来引入本地变量。下面就是let的一般形式，每月各变量都绑定了对应的值，之后主体才会求值。
(let ((变量名 值)…)
 主体…)
(let ((x 40)
   (y (+ 1 1)))
  (+ x y))
使用let定义本地变量和使用匿名函数定义参数是没有区别的:
((lambda (变量…)
 主体…)
 参数值…)
((lambda (x y)
  (+ x y))
40 
(+ 1 1))
首先，所有的值都会被求值。之后会绑定到每一个变量（lambda表达式的参数），最后主体会使用那些变量求值。
特殊形式let星，和let差不多，但是在绑定变量的时候会允许使用之前新定义的变量。
`(let* ((x 6)`
   `(y (* x x)))`
 `(+ x y))`
这历史不可以使用let的，因为计算y的值的时候用到了新定义的x。
######习题
【m】3.1 用lambda表达式写一个等同于let星效果的表达式。可能需要多个lambda。
由于列表在Lisp中的重要地位，专门准备了特殊形式来操作列表头部的元素添加和删除，也就是说用列表实现栈。如果list是一个列表的位置的名字，那么(push x list)会更改list，将x加到list的第一个元素，(pop list)会返回第一个元素，并且有一个副作用，就是删掉list的第一个元素。Push和pop等同于下面的表达式：
(push x list)
等同于
(setf list (cons x list))
(pop list)
等同于
(let ((result (first list)))
  (setf list (rest list))
 result)
列表可以对元素进行累加，一个和类似地可以对数字进行累加。Lisp提供incf和decf作为增加和减少一个数字的方法。第一个参数都必须是一个位置，之后的参数可以没有，默认是1 。C语言中是由自增运算符++的，功能和这个类似。
(incf x)等于(incf x 1)等于(setf x (+ x 1))
(decf x)等于(decf x 1)等于(setf x (- x 1))
当位置指向的不是一个变量而是更加复杂的形式的话，Lisp会小心的将不对人格子形式求值的代码扩展开。支持的有push，pop，incf和decf。接下来的例子中，我们有一组选手的列表，并要决定那一个选手的分数最高，赢得游戏。结构player由键值选手的分数和赢家的数目组成，函数determin-winner会在赢家的wins字段加上1.incf的扩展绑定了一个临时的变量所以分类没有完成两次。
(defstruct player (score 0) (wins 0))
(defun determine-winner (players)
  “Increment the WINS for the player with highest score.”
  (incf (player-wins (first (sort players #’>
   :key #’player-score)))))
等同于
(defun determine-winner (players)
  “Increment the WINS for the player with highest score.”
  (let ((temp (first (sort players #’>:key #’player-score))))
    (setf (player-wins temp) (+ (player-wins temp) 1))))
######用于迭代循环的函数和特殊形式
很多语言都有保留字用来支持迭代循环。例如，Pascal就有while，repeat和for语句。相对的Common Lisp有一个巨大的集合来选择。见下表：

名字|功能
---|---
Dolist|循环列表中的元素
Dotimes|循环连续的整数
Do,do星|一般循环，精简语法
Loop|一般循环，冗长语法
Mapc,mapcar|遍历列表（多个列表）的元素
Some，every|带条件的遍历列表
Find,reduce等等|更加具体的循环函数
递归|一般迭代

为了解释每一个选项，我们会介绍一个函数length，返回列表的元素个数。特殊形式dolist可以用来迭代列表的元素，语法是这样：
(dolist (变量 列表 可选结果) 主体…)
对于列表中的每一个元素，主体部分都会被执行一次，变量部分就是绑定在各个元素上，依次进行。最后dolist会求值并且返回可选结果表达式部分，如果没有结果表达式的话就返回nil。
下面是使用dolist来定义的length版本。形式let引入了一个新的变量，len，初始化绑定为0。Dolist会对应每一个列表的元素执行一次主体，并且给len加上1。这个实现的循环迭代变量element在主体中没有使用，这一般不大会这样。
(defun length1 (list)
  (let ((len 0))                     ;一开始len等于0
    (dolist (element list)        ;依次迭代
      (incf len))         ;len加1
   len))                    ;返回len
Dolist中也能用上可选结果部分，很多程序猿都使用这种风格，我发现这样有时很容易失去对结果的追踪，所以还是建议将结果在最后显式给出。
(defun length1.1 (list)                          ;可选结果的版本
  (let ((len 0))                           ;这种不是特意这样做的
    (dolist (element list len)           ;这里使用len作为结果返回
      (incf len))))
函数mapc的功能和特殊形式dolist是一样的。在最简单的情况下，mapc只有两个参数，第一个是函数，第二个是列表。将函数应用到列表的每一个元素上，下面是length的mapc版本：
(defun length2 (list)
 (let ((len 0))                       ;开始len是0
    (mapc #’(lambda (element)    ;迭代每一个元素
        (incf len))    ;len加上1
     list)
   len))                      ;返回len
一共有七个像map什么一样的定位函数，其中最常用的就是mapc和mapcar。Mapcar和mapc功能一样，只是最后的返回时一个结果的列表形式。
也有dotimes形式，语法是这样：
(dotimes (变量 数字 可选结果) 主体…)
主体的执行次数依据变量，变量一开始绑定到0，之后是1，一直到n-1，也就说是执行了n次。当然，dotimes不适合用来实现length，因为我们不确定到底循环多少次。
两个一般的循环格式，do和loop。Do的语法：
(do ((变量 初始值 递进部分)…)
(脱离测试 结果)
主体…)
每一个变量都是绑定初始值来初始化。如果脱离测试部分是真，那么结果就会被反悔。另外，主体执行一次，变量就会对应递进部分设定一次，然后再次尝试脱离测试。这个循环一直尺度到脱离测试部分为真。如果递进部分被省略，对应的递进部分就不会更新。这样子的话就和let没有区别了。
用do实现的length，引入了两个变量，len用来存储列表的元素个数，l用来接收列表。L经常会说是探底列表，因为他会一直对列表使用cdr操作。（实际上这里我们用更加容易记忆的rest来代替cdr。）请注意do循环是没有主体的。所有的计算都在变量初始化和递进阶段完成，在测试部分结束。
(defun length3 (list)
  (do ((len 0 (+ len 1))                   ;开始len等于0，递增
    (l list (rest l)))                 ;一次次迭代
    ((null l)len)))                  ;直到列表的尽头
Do是很难让人明白，因为他没有清晰的说明我们正在循环到哪里了。为了确认迭代列表的进度就需要查看l和测试部分。没有变量来表示当前的元素在列表什么位置，我们必须用(first l)来代替查看。Dolist和mapc都设定了步进，测试和变量自动命名。就具体这个原则来说，都的确是不是很具体，所以本书中我们不太会使用do。但是很多优秀的程序员喜欢使用do，所以即使我们不适用do，也要看明白do的运行。
Loop的语法有他自己的一整套语言，并且是和Common Lisp不类似的风格。我们不会列出loop的所有特性，仅仅给出一些例子，具体请参考CLTL第24章5小节。下面是一些使用loop实现的length版本：
(defun length4 (list)
 (loop for element in list;遍历每一个元素
   count t));计数
(defun length5 (list)
  (loop for element in list; 遍历每一个元素
   Summing 1));加一
(defun length6 (list)
(loop with len = 0;开始len是0
Until (null list);直到列表为空
For element = (pop list);依次迭代
Do (incf len);len加1
Finally (return len)));返回len
每一个程序员都有关于循环的一些经验，反复的实践过。经常就叫做编程惯用语法或者陈词滥调。其中一个例子就是遍历列表或者数组中的每一个元素然后做出相应的操作。在大部分语言中，这些惯用用法不会有一个显式的语法标记，一般而言是用一个普通的循环结构实现，然后让读者来理解分辨程序员在做什么。
Lisp的不同之处在于他提供的方式是显式地封装了这些惯用语法，并且用成型语法和特殊形式来指向他们。Dolist和dotimes就是两个例子，都是遵循了“具体化”的原则。大部分程序员倾向于使用dolist而不是do。
除了特殊形式dolist和dotimes之外，还有一些函数是设计来处理一般的惯用语法的。比如count-if，就是计算序列中满足断言的元素的个数，还有position-if，返回满足断言的元素的索引值。两者都备用来实现length。在线面的length7，count-if给出了在list中满足断言的元素个数。函数断言true的值总是真，元素都满足，那么就会返回list的长度。
(defun length7 (list)
(count-if #’true list))
(defun true (x) t)
在length8中，函数position-if找到一个满足断言的元素的位置，位置计算是从列表的最后开始的。既然索引是从0开始，那么长度就需要加上1得到。必须承认，这不是一个最直接的length实现方法。
(defun length8 (list)
(if (null list)
0
(+ 1 (position-if #’true list :form-end t))))
一些函数的灵活设计可以让函数处理急速所有的序列操作。这种灵活性主要是三种形式：第一，mapcar一类的函数可以操作任意数量的列表：
`> (mapcar #'- '(1 2 3)) => (-1 -2 -3) `
`> (mapcar #'+ '(1 2) '(10 20)) => (11 22) `
`> (mapcar #'+ '(1 2) '(10 20) '(100 200)) => (111 222)`
第二，很多函数是接受用户定义的关键字，可以更改函数的比较测试功能，或者是只操作获取序列的一部分。
> (remove 1 '(1 23210 -1)) => (2 3 2 a -1) 
> (remove 1 '(1 23210 -1) :key #'abs) => (2 3 2 0) 
> (remove 1 '(123210 -1) :test #'<) => (1 1 a -1) 
> (remove 1 '(123210 -1) :start 4) => (1 2 3 2 a -1)
第三，一些对应的函数是以-if结尾或者-if-not结尾，就会接受一个断言而不是一个元素作为匹配：
> (remove-if #'oddp '(1 2 3 2 1 a -1)) => (2 2 0) 
> (remove-if-not #'oddp '(1 2 3 2 1 a -1)) => (1 3 1 -1) 
> (find-if #'evenp '(1 2 3 2 1 a -1)) => 2
下面的两张表有着前提：
(setf x ‘(a b c))
(setf y ‘(1 2 3))
第一张表列出的函数是可以处理任意数量的列表，但是不接受关键字选项。

表达式|结果|说明
---|---|---
(every #’oddp y)|nil|测试是不是每一个元素都符合断言
(some #’oddp y)|t|测试一些元素是不是符合断言
(mapcar #’- y)|(-1 -2 -3)|将函数应用到每一个元素，然后返回结果
(mapc #’print y)|打印1 2 3|对每一个元素执行操作

第二张表列出的函数是由-if和-if-not的版本的函数，也接受关键字函数：

表达式|结果|说明
---|---|---
(member 2 y)|(2 3)|看看元素是不是在列表中
(count ‘b x)|1|计算匹配的元素数量
(delete 1 y)|(2 3)|输出的时候省略匹配的元素
(find 2 y)|2|找到匹配的第一个元素
(position ‘a x)|0|在序列中找出元素的索引
(reduce #’+ y)|6|将函数应用到后续的元素
(remove 2 y)|(1 3)|和delete类似，但是会做一份新的拷贝
(substitute 4 2 y)|(1 4 3)|用新元素替代旧元素

######用递归来迭代
Lisp被称作是一种递归的语言，意思就是Lisp鼓励开发者使用函数的时候调用自身。正如上面缩进啊，在Common Lisp中有一些令人目眩的函数和特殊形式来写循环，但是也是，很多程序处理迭代的时候都不是用循环而是使用递归的。
Length的一个简单定义就是空列表的长度为0，其他的列表都是自己的rest的长度加上1.这个定义就可以直接转化成递归代码。
(defun length9 (list)
  (if (null list)
   0
    (+ 1 (length9 (rest list)))))
这个版本的length定义和容易得出关于列表定义的递归型结论：一个列表要么事空列表，要么就是一个元素和另一个列表的组合。一般来说，大部分递归函数都是由他们要处理的数据的递归本质所衍生出来的。有些数据，比如二进制树，用递归函数以外的函数是很难处理的。另外像列表和整型，既可以用递归定义（紧接这就用递归函数），也可以序列定义（就会使用迭代函数）。在本书中，我们倾向于用列表是一个序列的观点，而不是元素和列表的组合的观点。采取这种观点的理由是列表作为一个first元素和rest列表的分割是一种认为的任意的分割，这种分割基于列表的实现，而且实现的方式根据Lisp的不同而不同。但是还有很多其他的方式来分解一个列表。我们可以将列表看做最后一个元素和其他元素的组合，也可以看做是前半部分和后半部分的组合。列表作为一个序列的观点就会去除人为定义的因素。将所有的元素平等对待。
由于低效，递归函数的使用也有一些反对的声音，因为编译器一定要给每一个递归调用分配内存。对于length9，这的确是对的，但是并不是所有的递归函数调用都这样。看看下面的函数：
(defun length10 (list)
  (length10-aux list 0))

(defun length10-aux (sublist len-so-far)
  (if (null sublist)
   len-so-far
    (length10-aux (rest sublist) (+ 1 len-so-far))))
Length10使用length10-aux作为一个辅助函数，将0作为列表的长度传递进去。Length10-aux之后会对list进行遍历，对每一个元素加1。这里有个不变的关系就是子列表的长度加上len-so-far的值总是等于原始列表的长度的。因此，当子列表变为空列表，len-so-far就是原始列表的长度。像len-so-far这种变量，执行追踪结果的功能，一般被称作收集器。其他使用收集器的函数例子包括之后的flatten-all函数，one-unknown函数。还有之后讨论的Prolog断言，在后面的anonymous-variables-in中我们使用了两个收集器。
Length9和length10之间的区别在于什么时候做加1。在length9中，函数调用本身，之后会返回，然后加1。在length10-aux中，函数是加1，然后调用自身，之后返回。在调用自身之后没有任何其他的操作，所以编译器可以将在递归之前原始调用分配的内存全部释放，length10-aux这种递归函数就叫做尾递归，因为递归调用出现在函数的最后，被称作尾递归。很多遍一起都会优化尾递归，但是所有的编译器都会这么做。（第22章会更加详细的介绍尾递归，而且在Scheme的便以其中，尾递归的编译器优化是一定会做的）。
有些人会觉得引入辅助函数length10-aux是一种很不优雅的写法，这里我们提供两个其他的选择。第一种，我们可以结合length10和length10-aux，写到一个函数中，这需要使用一个可选参数：
(defun length11 (list &optional (len-so-far 0))
  (if (null list)
   Len-so-far
    (length11 (rest list) (+ 1 len-so-far))))
第二种，我们可以在主函数本身的定义中引入一个本地函数。这可以用特殊形式labels实现：
(defun length12 (the-list)
  (labels 
    ((length13 (list len-so-far)
      (if (null list)
       Len-so-far
        (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))
一般来说，labels形式（或者相似的flet形式）可以用来引入一个或多个本地函数。语法：
(labels
((函数名 (参数…) 函数体)…)
Labels的主体)
######其他特殊形式
有一些特殊形式并不能够进行严格的分类。我们看到了两中创建常量和函数的特殊形式，quote和function。他们是有简略形式的：单引号加上x代表(quote x),井号单引号f表示(function f)。
特殊形式progn可以用来对形式的序列求值，返回最后一个的值。
`(prong (setf x 0) (setf x (+ x 1)) x) => 1`
Prong在其他语言中相当于begin和end区块，但是在Lisp中不是很常用。主要是两个原因，第一，用函数式风格写就的程序不需要序列操作，因为他们没有副作用。第二，即使需要副作用，很多特殊形式的主体本身就是一个隐式的progn。Progn的使用我能想到的有三个地方。第一是条件分支的两个选择之一，可以在if中使用progn或者cond：
`(if (> x 100)`
   `(progn (print “too big”)`
   `(setf x 100))`
 `x)`

`(cond ((> x 100)`
   `(print “too big”)`
   `(setf x 100))`
 `(t x))`
如果条件式只有一个分支，那么就应该使用when或者unless，他们都隐含了progn。如果有超过两个分支，就应该用cond。
第二，progn在宏之中有需要使用，用来扩展成多个顶层形式，就像在defun星中的例子。第三，有时候progn会用在unwind-protect，一种高级宏。之后会介绍一个例子，一个高级宏with-resource。
Trace和untrace用来控制函数调用和返回的调试信息。
`> (trace length9) => (LENGTH9) `
`> (length9 '(a b c) => `
`(1 ENTER LENGTH9: (A B C)) `
 `(2 ENTER LENGTH9: (B C)) `
   `(3 ENTER LENGTH9: (C))) `
     `(4 ENTER LENGTH9: NIL) `
     `(4 EXIT LENGTH9: 0) `
   `(3 EXIT LENGTH9: 1) `
 `(2 EXIT LENGTH9: 2) `
`(1 EXIT LENGTH9: 3) `
`3 `
¬`> (untrace length9) => (LENGTH9) `
`> (length9 '(a b c)) => 3`
最后，特殊形式return可以用来从代码块中返回。块使用特殊形式block来设置的，或者是用循环形式（do，do星，dolist，dotimes，或者loop）。例如，下面的函数计算的是列一个表中数字的乘积，但是如果数字中包含了0，那么乘积就肯定是0，所以就从dolist循环中返回0.请注意这里只是从dolist中返回，不是函数本身返回（虽然有可能dolist是函数的最后一个语句，返回值也就是函数的返回值）。一般要使用大写的RETURN来标识这里是循环的出口。
(defun product (numbers)
 “Multiply all the numbers together to compute their product.”
  (let ((prod 1))
    (dolist (n numbers prod)
      (if ( = n 0)
        (RETURN 0)
        (setf prod (* n prod))))))
######宏
接下来的讨论是一些对于术语特殊形式的漫谈。实际上这些特殊形式中的一些是用宏还是先的，编译器会负责进行扩展。Common Lisp提供了内建宏摒弃诶允许定义新的宏来扩展语言。（但是想定义新的特殊形式是不可能的）
宏是用特殊形式defmarco来定义的。结社我们想要定义一个宏，while，行为和Pascal中的while循环语句一样。写一个宏的过程分为4步：
考虑一下是不是真的需要这个宏
写下宏的语法
指明宏应该扩展成什么样子
使用defmarco来实现对应的语法
第一步就是考虑需不需要一个新的宏，因为这就是和定义一门新的语言一样。这种看法使得开发者在定义新的宏的时候极端谨慎。（另外，当有人问起，“你今天干了些什么？”，说，“定意义了一门新的语言吗，并写了一个编译器”肯定要比回答“写了一些宏”要拉风很多）。引入一个宏要比引入一个函数，给你的程序的阅读者增加的负担大得多，所以不要轻易定义宏。只有当非常确定需要宏的时候，考虑引入宏会和现有的系统完美配合。就如C.A.R.Hoare说过，“语言设计不应该做得事情就是引入自己没有考虑过的概念。”
第二步就是考虑宏应该展开成什么。无论如何，遵循已有的Lisp约定俗成的宏语法是一个好的习惯。看一下用于循环的宏（dolist，dotimes，do-symbols），用于定义的宏（defun，defvar，defperameter，defstruct），或者是I/O宏（wit-open-file，with-open-system，with-input-from-string）。如果你遵循命名和语法的规则，而不是自行其是的话，对于你的代码的阅读者会省心很多，你自己看代码也会省心很多。对于while，一个好的语法就是：
(while test body…)
第三步就是写下宏调用的展开代码：
(loop
 (unless test (return nil))
body)
最后一步就是使用defmarco写下宏的定义。defmarco形式很接近于defun，也有参数列表，可选文档字符串和主体。也有一些区别，在参数列表的处理上有区别，之后我们会解释。下面是一个宏while的定义，有一个test部分和一个body部分，用的是之前loop代码：
`(defmarco while (test &rest body)`
 `“Repeat body while test is true.”`
 `(list* ‘loop`
   `(list ‘unless test ‘(return nil))`
   `body))`
（这个list星函数和list函数很像，除了一点，他会将最后一个参数追加到其他参数的列表的末尾）通过使用macroexpand，我们可以看到宏的展开形式：
`> (macroexpand-1 '(while < i 10) `
`(print (* i i)) `
`(setf i (+ i 1)))) =>`
`(LOOP (UNLESS < I 10) (RETURN NIL)) `
`(PRINT (* I I)) `
`(SETF I (+ I I))) `
`¬> (setf i 7) => 7 `
`> (while (< i 10)` 
`(print (* i i)) `
`(setf i (+ i 1))) => `
`¬49 `
`64 `
`81 `
`NIL`
之后我们会描述一个更加复杂的宏，和写宏的时候容易出现的陷阱和错误。
######反引号标记
定义宏while最难的部分就是构建宏扩展的代码。如果有更加方便的方法也是不错的，下面的while版本就是这么个尝试。他定义一个本地变量code作为我们最终代码的模板，之后用实际的代码来代替代码中的test和body等占位符号。这个用函数subst实现(subst new old tree)，这个语句会把每一个old在tree中出现的地方替代成new。
(defmarco while (test &rest body)
  “Repeat body while test is true.”
  (let ((code ‘(loop (unless test (return nil)) . body)))
  (subst test ‘test (subst body ‘body code))))
用组件构建代码或者非代码数据，经常需要一种特殊的标记，就是反引号标记。反引号字符“`”和引号字符“’”很像。一个反引号表示接下来基本上是一个字面意义上的或许包含了一些需要求值的部分的表达式，任何标记有前导逗号“,”的符号都会被求值，之后插入到结构中，任何有前导符号“,@”的符号必须求值一个由分隔符分割结构的列表：被插入到每一个元素之间，没有顶层括号。标记的细节我们在第23.5小节介绍。下面是使用了反引号和逗号重写的while：
`(defmarco while (test &rest body)`
 `“Repeat body while test is true.”`
 ``(loop (unless ,test (return nil))`
   `,@body))`
下面是一些反引号的例子。请注意列表的结尾，“,@”的效果和“.”之后跟上一个逗号“,”的效果是一样的。在列表中间，只可以使用“,@”。
`> (setf testl '(a test)) => (A TEST) `
`¬> `(this is ,testl) => (THIS IS (A TEST)) `
`> `(this is ,@testl) => (THIS IS A TEST) `
`> `(this is ,testl) => (THIS IS A TEST) `
`¬> `(this is ,@testl -- this is only ,@testl) => (THIS IS A TEST -- THIS IS ONLY A TEST)  `
这一小节就完整介绍了特殊形式和宏。本章的其余小节就介绍Common Lisp中重要的内建函数。
#####3.3 操作列表的函数
为了举例子方便，我们先做如下假设：
(setf x ‘(a b c))
(setf y ‘(1 2 3))
我们在这里总结一下列表操作中最重要的函数，更加复杂的一些函数我们用到的时候再解释。

函数的表达式|求值结果|功能
---|---|---
(first x)|=>a|列表的第一个元素
(second x)|=>b|列表的第二个元素
(third x)|=>c|列表的第三个元素
(nth 0 x)|=>a|从0开始数，列表的第n个元素
(rest x)|=>(b c)|除了第一个元素之外的所有元素
(car x)|=>a|first的别名
(cdr x)|=>(b c)|rest的别名
(last x)|=>(c)|列表中的最后一个组合单元
(length x)|=>3|列表中元素的数目
(reverse x)|=>(c b a)|元素逆序的列表
(cons 0 y)|=>(0 1 2 3)|在列表的最前面加上元素
(append x y)|=>(a b c 1 2 3)|聚合列表的元素
(list x y)|=>((a b c) (1 2 3))|创建一个新列表
(list* 1 2 x)|=>(1 2 a b c)|将最后一个参数追加到其他元素后面
(null nil)|=>T|是否为空断言为真
(null x)|=>nil|是否为空断言为假
(listp x)|=>T|是否为列表断言为真
(listp 3)|=>nil|是否为列表断言为假
(consp x)|=>t|是否为列表断言为真
(consp nil)|=>nil|是否为原子断言为假
(equal x x)|=>t|列表相等为真
(equal x y)|=>nil|列表不等为假
(sort y #’>)|=>(3 2 1)|根据比较函数对列表排序
(subseq x 1 2)|=>(B)|根据给出的起始点和结束点截取子序列

表达式(cons a b)会将元素a加到列表b的前面，形成一个更长的列表，但要是b不是一个列表呢？其实也不会出错的，它的结果我们假设是一个叫做x的对象，对象的性质(first x)结果是a，(rest x)的结果是b，x的打印形式就是(a . b)。这个标记被称作点对标记。如果b是一个列表，那么普通的列表打印就可以用了，但是两种标记都不能同时用来输入。
现在为止，我们使用列表就是序列的观点，用的是“三个元素的列表”这样的意思。列表是一个很方便的抽象模型，但是实际上底层的列表的实现是构筑在叫做组合单元的基础上。一个组合单元就是一种具有两个域的数据结构：头和尾。一个三元素的列表，也可以看做一个组合单元，头指向的是第一个元素，尾指向的下一个单元，第二个单元的头指向第二个元素，尾指向第三个单元，第三哥哥单元的头指向第三个元素，尾是nil。所有普通的列表的尾都是nil。下图表示的就是这种数据结构。
![cons cell观点下的list.JPG](http://upload-images.jianshu.io/upload_images/46495-d85bf5fddb019a08.JPG)
此图表示的是三元素列表的内部结构，还有(cons ‘one ‘two)的结果结构。
######习题
【s】函数cons可以看做是之前列出的函数的一个特殊情况，哪一个？
【m】写一个打印点对标记的函数。使用内建函数princ来的打印表达式的每一个组件。
【m】写一个函数，类似于常规的print函数，需要时打印常规的点对表达式，但是也可以打印列表表示形式。
