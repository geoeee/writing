#####6.2 模式匹配工具
函数pat-match是为了ELIZA程序特别定制的模式匹配器。随后的程序也需要模式匹配器，其实并不需要为每一个程序定制模式匹配器，可以定义一个通用的模式匹配器来满足大部分需要，然后根据情况自己扩展。
设计一个通用工具的问题在于，应该包括哪些特性？我们可以尝试来定义一些可能会有用的特性，但是也可以创建一个可扩充的特性的列表，新的特性可以方便的添加和移除。
要添加的特性可以通过归纳或者定制现有的特性来实现。例如，我们提供过的匹配零个或多个输入元素的段变量。我们可以通过提供一种匹配一个或多个元素的段变量来定制，或者用一个匹配零个或多个元素的可选变量。另一种可能是生成一个段变量来定制一个m元素到n个元素的匹配，也就是对于任意的m和n。这些想法主要来自于对一般表达式定时时候的表示法，也来自于启发式的总结，比如这种，需要考虑的重要的特殊情况和零个或一个例外存在的特殊情况。
另一个有用的特性就是允许用户定义一个任意的断言来满足匹配条件。标记法(?is ?n numberp)被用来匹配任何数字或者绑定到变量？n上的表达式：
`> (pat-match '(x = (?is ?n numberp)) '(x = 34)) => ((?n . 34)) `
`> (pat-match '(x = (?is ?n numberp)) '(x = x)) => NIL`
 既然模式是类似于布尔表达式，那么允许布尔操作符来操作也就是合理的了。根据这种问号标记的惯例，我们使用？和？or还有？not来表示操作符。另一个比较好的选择是使用关键字来表示，比如：and，：or和：is。下面的模式是匹配一个关系表达式，他会怕判断三个其中的一个。表达式成功的原因是小于号满足了定义的三种可能性中的一个。
`> (pat-match '(?x (?or < = > ?y) '(3 < 4)) => ((?Y . 4) (?X . 3))`
下面是一个？and模式来检查表达式是不是一个数字，并且是一个奇数：
`> (pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) `
`'(x = 3)) `
`¬((?N . 3))`
下一个模式是使用？not来确保两部分是不等的：
`> (pat-match '(?x /= (?not ?x)) '(3 /= 4)) => ((?X . 3))`
段匹配标记我们之前已经见过了。扩展成了允许三种可能性：零个或多个表达式，一个或多个表达式还有零个或一个表达式。最后，标记大(?if exp)可以用来测试一些变量之间的关系。作为一个段模式而不是单个模式必须用列表来列出，因为他不消耗任何输入：
`> (pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)) =>`
`((?Y . 3) (?X . 4))`
当问题的描述变得越来越复杂，尝试使用一个更加正规的定义是很必要的。下面的表格描述了一种模式的语法，和第2章描述的语法规则格式相同。

匹配模式|匹配种类|功能含义
---|---|---
Pat=>|var|匹配任何单个的表达式
 |constant|仅匹配这个原子
 |segment-pat|对应序列匹配一些东西
 |single-pat|针对单个表达式匹配一些东西
 |(pat . pat)|匹配first和rest
Single-pat=>|(?is var predicate)|对单个表达式的测试断言
 |(?or pat…)|在单个表达式上匹配任一模式
 |(?and pat…)|在单个表达式上匹配任意模式
 |(?not pat…)|如果模式不匹配则返回真
Segment-pat=>|((?* var) …)|匹配零个或多个表达式
 |((?+ var) …)|匹配一个或多个表达式
 |((?? var) …)|匹配零个或多个表达式
 |((?if exp) …)|测试表达式（可能含有变量）是否为真
Var=>|?chars|问号开头的符号
Constant=>|atom|任何非变量的原子

不管附加的复杂性的话，所有的模式都可以分类成为5类。模式必须是5类之一，变量，常量，一个（广义上的）段模式，一个（广义上的）单元素模式，或者是两个模式的组合。下面pat-match的定义反映了这五种情况（附带两次失败检查）：

(defun pat-match (pattern input &optional (bindings no-bindings))
 “Match pattern against input in the context of the bindings”
  (cond ((eq bindings fail) fail)
    ((variable-p pattern)
    (match-variable pattern input bindings))
    ((eql pattern input) bindings)
    ((segment-pattern-p pattern)
    (segment-matcher pattern input bindings))
    ((single-pattern-p pattern)
    (single-matcher pattern input bindings))
    ((and (consp pattern) (consp input))
    (pat-match (rest pattern) (rest input)
      (pat-match (first pattern) (first input)
       bindings)))
    (t fail)))

完整起见，我们在这里重复一下来自于ELIZA的必须的常量和低层函数：
`(defconstant fail nil “Indicates pat-match failure”)`

`(defconstant no-bindings ‘((t . t))`
`“Indicates pat-match success, with no variables.”)`

`(defun variable-p (x)`
`“Is x a variable (a symbol beginning with ‘?’)?”`
`(and (symbol x) (equal (char (symbol-name x) 0) #\?)))`

`(defun get-binding (var bindings)`
`“Find a (variable . value) pair in a binding list.”`
`(assoc var bindings))`

`(defun binding-var (binding)`
`“Get the bariable part of a snigle binding.”`
`(car binding))`

`(defun binding-val (binding)`
`“Get the value part of a single binding.”`
`(cdr binding))`

`(defun make-binding (var val) (cons var val))`

`(defun lookup (var bindings)`
`“Get the value part (for var) from a binding list.”`
`(binding-val (get-binding var bindings)))`

`(defun extend-bindings (var val bindings)`
`“Add a (var . value) pair to a binding list.”`
`(cons (make-binding var val)`
 `;; Once we add a “real” binding.`
 `;; we can get rid of the dummy no bindings`
 `(if (eq bindings no bindings)`
   `Nil`
   `bindings)))`

`(defun match-variable (var input bindings)`
`“Does VAR match input? Uses (or updates) and returns bindings.”`
`(let ((binding (get-binding var bindings)))`
 `(cond ((not binding) (extend-bindings var input bindings))`
   `((equal input (binding-val binding)) bindings)`
   `(t fail))))`

下一步就是来定义断言来识别广义上的段模式和单元素的短模式，并且还要定义操作他们的匹配函数。我们可以试下segment-matcher和single-matcher来包括这些可能的情况。然而，这将会为我们扩展匹配器制造困难。想要增加新段模式种类的程序员只好重新编辑segment-pattern-p和segment-matcher的定义来使用心得特性。这本身或许不是大事儿，但是要考虑到两个程序员都会独立添加新特性的情况，如果都想要使用的话，之后任一版本的segment-matcher或者segment-pattern-p都要能用。你将不得不在一次编辑函数，仅仅是为了合并两个扩展。
解决这个困境的办法就是为segment-pattern-p和segment-matcher各写一个单独的版本，并且搞一个这些函数的指向表来表示模式和行为的对。这个表会说，看见了什么什么模式就调用什么segment-matcher，等等。想要扩展匹配器的程序员只要在表中添加入口，就可以合并不同的扩展了（除非说两个程序员对不同的行为选择了同一个符号）。
这种把模式-动作的对存储在一张表中的编程风格被称作数据驱动编程。在写可扩展系统的时候这是一种很灵活的风格。
如我们之前章节中见过，Common Lisp中有很多方式可以实现表格。在这里，表格的关键是符号的（如？标记），并且符号的表现形式是通过内存分发的，所以属性列表会是一个合适的选择。我们有两张表格，表现为符号的segment-match属性和single-match属性。下面是之前列出的实现语法的表格入口：
`(setf (get '?is 'single-match) 'match-is) `
`(setf (get '?or 'single-match) 'match-or) `
`(setf (get '?and 'single-match) 'match-and) `
`(setf (get '?not 'single-match) 'match-not) `
`(setf (get ' ?* 'segment-match) 'segment-match) `
`(setf (get '?+ 'segment-match) 'segment-match+) `
`(setf (get '?? 'segment-match) , segment-match?) `
`(setf (get '?if 'segment-match) 'match-if)`
随着表格的定义，我们需要做两件事情。首先，定义聚合表格的胶合层：断言和操作函数。寻找并调用一个数据驱动函数的函数（比如segment-matcher进而single-matcher），被称作是调度函数。

`(defun segment-patern-p (pattern)`
`“Is this a segment-matching pattern like ((?* var) . pat)?”`
`(and (consp pattern) (consp (first pattern))`
 `(symbol (first (first pattern)))`
 `(segment-match-fn (first (first pattern)))))`

`(defun single-pattern-p (pattern)`
`“Is this a single-matching pattern?`
`E.g. (?is x predicate) (?and . patterns) (?or . patterns).”`
`(and (consp pattern)`
 `(single-match-fn (first pattern))))`

`(defun segment-matcher (pattern input bindings)`
`“Call the right function for yhis kind of segment pattern.”`
`(funcall (segment-match-fn (first (first-pattern)))`
 `Pattern input bindings))`

`(defun single-matcher (pattern input bindings)`
`“Call the right function for this kind of single pattern.”`
`(funcall (single-match-fn (first pattern))`
 `(rest pattern) input bindings))`

`(defun segment-match-fn (x)`
`“Get the segment-match function for x,`
`If it is a symbol that has one.”`
`(when (symbol x) (get x ‘segment-match)))`

`(defun single-match-fn (x)`
`“Get the single-match function for x,`
`If it is a symbol that has one.”`
`(when (symbolp x) (get x ‘single-match)))`

最后要做的事情是定义独立的匹配函数。首先，是single-pattern的匹配函数：
`(defun match-is (var-and-pred input bindings)`
`“Succeed and bind var if the input satisfies pred,`
`Where var-and-pred is the list (var pred).”`
`(let* ((var (first var-and-pred))`
  `(pred (second var-and-pred))`
  `(new-bindings (pat-match var input bindings)))`
 `(if (or (eq new-bindings fail)`
     `(not (funcall pred input)))`
   `Fail`
   `New-bindings)))`

`(defun match-and (patterns input bindings)`
`“Succeed if all the patterns match the input.”`
`(cond ((eq bindings fail) fail)`
 `((null patterns) bindings)`
 `(t (natch-and (rest patterns) input`
   `(pat-match (first patterns) input`
     `bindings)))))`

`(defun match-or (patterns input bindings)`
`“Succeed if any one of the patterns match the input.”`
`(if (null patterns)`
 `Fail`
 `(let ((new-bindings (pat-match (first patterns)`
       `Input bindings)))`
   `(if (eq new-bindings fail)`
     `(match-or (rest patterns) input bindings)`
     `New-bindings))))`

`(defun match-not (patterns input bindings)`
`“Succeed if none of the patterns match the input.`
`This will never bind any variables.”`
`(if (match-or patterns input bindings)`
 `Fail`
 `bindings))`
