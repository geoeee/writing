#####第6章 构建软件工具
所谓人，就是能够使用工具的动物。没有工具就无从着力，有了工具则所向披靡。
——托马斯•卡莱尔（1795-1881）
在第4章和第5章，我们主要是构建了两个特定程序，GPS和ELIZA。在本章，我们来复习一下这两个程序，来找找看一些普遍的模式。这些从可重用软件工具中抽象出来模式将会在后续章节中起到一些帮助作用。
#####6.1 一个交互式的解释器工具
这个函数结构是eliza的一个通用版本，这里再提点一下：
`(defun eliza ()`
 `“Respond to user input using pattern matching rules.”`
 `(loop`
   `(print ‘eliza>)`
   `(print (flatten (use-eliza-rules (read))))))`

其他很多应用都是用了这种模式，包括Lisp本身。Lisp的顶层表现可以定义成这样子：
`(defun lisp ()`
 `(loop`
   `(print ‘>)`
   `(print (eval (read)))))`

一个Lisp系统的顶层表现历史上一般就称作“读取-求值-打印 循环”这样的过程。大部分Lisp会在读取输入之前打印一个提示符，所以实际上应该是“提示符-读取-求值-打印 循环”才对，但是在一些早期的系统中，比如MacLisp就是没有提示符的。如果我们不考虑提示符的话，也可以仅用四个符号就写出一个完整的Lisp解释器：
(loop (print (eval (read))))
仅仅用这四个符号和八个括号就像构建一个Lisp解释器，听上去是像是在开玩笑。那这一行代码，到底是写出了什么意思呢？这个问题的一个答案就是去想象，如果用Pascal语言来写一个Lisp或者Pascal的解释器，我们究竟要做什么。需要一个词法分析器和一个符号表管理器。这些都是在工作的范围内，但是read可以处理这些东西。还需要语法分析器来聚合词法分隔符形成语句。Read也把这活儿干了，但只是因为Lisp的语句的语法是任意的，也就是列表和原子的语法。因此read在Lisp中扮演了一个很好的语法分析器的角色，但是在Pascal中是不行的。接下来，就是解释器的求值或者解释部分；eval完成了这项功能，并且也可以像处理Lisp表达式那样处理好Pascal的语句。Print所做的要比read和eval少得多，但是仍然是很有必要的。
重点并不是在于一行代码就可以被看做是一个Lisp的实现，而是要看做是计算过程的一般模式。ELIZA和Lisp都可以看做是交互式的解释器，读取一个输入，以某种方式转化或者求值输入，打印结果，之后返回等待更多的输入。我们从中可以提取出如下的一般模式：
(defun program ()
 (loop
    (print prompt)
    (print (transform (read)))))
有两种方式来利用这些递归模式：正规路子和野路子。先说野路子，将模式看做一个模板或是一种泛型，在程序设计过程中根据应用的不同屡次使用。当我们要写一个新程序，我们回想起写过的或者看到过的相似的程序，回头看看那个程序，吧相关的部分留下，之后修改为新程序做些修改就可以了。如果借用的程序部分比较多的话，在新程序中用注释标记一下原始程序的部分是一个比较好的做法，但是在原始程序和导出程序之间，是没有什么“官方”的连接的。
正规路子就是创建一种抽象，以函数的形式或者以数据结构的形式，显式地指向每一个新的应用——就是说，以一个可用软件工具的形式来适应抽象。解释器模式可以被抽象成一个如下的函数：
(defun interactive-interpreter (prompt transformer)
 “Read an expression, transform it, and print the result.”
  (loop
    (print pronmpt)
    (print (funcall transformer (read)))))
这个函数可以用来写每一个新的解释器：
`(defun lisp ()`
`(interactive-interpreter ‘> #’eval))`

`(defun eliza ()`
`(interactive-interpreter ‘eliza>`
`#’(lambda (x) (flatten (use-eliza-rules x)))))`
或者，可以借用高阶函数compose：
`(defun compose (f g)`
`“Return the function that computes (f (g x)).”`
`#’(lambda (x) (funcall f (funcall g x))))`

`(defun eliza ()`
`(interactive-interpreter ‘eliza>`
`(compose #’flatten #’use-eliza-rules)))`

在正规路子和野路子之间有两个主要的区别。首先，他们看上去不一样。如果是一个简单的抽象，就像上面那个，读取一个有显式循环输入发热表达式要比读取一个调用interactive-interpreter的表达式简单得多，因为后者需要找到interactive-interpreter的定义，还要理解定义才可以。
另一个区别在维护性上体现出来。假设我们在交互式解释器的定义中遗漏了一个特性。比如说疏忽了Loop的出口。我们就需要假定，用户可以用一些中断信息按键来结束循环。一个比较干净的实现是允许用户给解释器一个显式的结束命令。另一个有用的特性就是在解释器内除处理错误。如果我们使用野路子，给程序添加一个这样的特性就不会影响其他程序了。但是如果我们使用正规路子，之后对interactive-interpreter的所有改动将会自动给所有使用它的程序带来新的特性。
后面的interactive-interpreter版本增加了两个新的特性。首先，他使用宏handler-case来处理错误。这个宏会先求值第一个参数，然后返回第一个参数的值。但是如果有错误发生的话，后面的参数就会根据已发生的错误进行错误条件检查。这么用的话，error会匹配所有的错误，才去的行动就是打印错误条件之后继续。
这个版本也允许提示字符串或者一个没有参数的函数，函数会被调用打印提舒服。函数prompt-generator，会返回一个函数来打印形式1,2等等的提示符。
`(defun interactive-interpreter (prompt transformer)`
 `“Read an expression, transform it, and print the result.”`
 `(loop`
   `(handler-case`
     `(progn`
       `(if (string prompt)`
         `(print prompt)`
         `(funcall prompt))`
       `(print (funcall transformer (read))))`
     `;; In case of error, do this:`
     `(error (condition)`
       `(format t “~&;; Error ~a ignored, back to top level.”`
         `condition)))))`

`(defun prompt-generator (&optional (num 0) (ctl-string “[~d] ”))`
`“Return a function that prints prompts like [1], [2], etc.”`
`#’(lambda () (format t ctl-string (incf num))))`

