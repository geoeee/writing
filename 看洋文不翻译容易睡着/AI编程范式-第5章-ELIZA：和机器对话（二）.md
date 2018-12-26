#####5.1 描述和定义ELIZA
现在，我们已经有了ELIZA的一个大概概念，我们可以开始描述和定义程序了，之后可以开始实现和调试。
ELIZA的算法可以简单描述为这样：（1）读取一个输入（2）找到一个匹配模式的输入（3）将输入转化成一个回答（4）打印回答。这四步会在每一个输入中循环往复。
步骤1到步骤4的实现和定义顺序是任意的：对于1，使用内建的read函数来读取一个单词的列表，第四步就是用print来打印回复的单词的列表。
当然，这个定义还是有缺点的。用户必须使用列表的形式，带括号的输入，用户也不可以使用read不能读取的符号，比如引号，逗号，顿号。所以我们的输入不会是随心所欲的，但是这种对于方便的小小牺牲可以让问题更好解决一些。
#####5.2 模式匹配
最难的部分就是第二步和第三步，模式匹配的表示和转化。这里有四件事情需要考虑：一个通用的模式和回复，还有一个特定的输入和对输入的转化。既然我们将输入作成一个列表，那么其他的组件也应该是列表形式的，例如

模式：(I need a X)
回复：(what would it mean to you if got a X ?)

输入：(I need a vacation)
转化：(what would it mean to you if you got a vacation ?)

模式匹配，就必须在字面意义上对i匹配i，need匹配need，a匹配a，对变量X就匹配vacation。先决条件就是有一些方式决定了X是一个变量而need就不是。我们必须安排vacation在回复中替代X，来得到最终的转化。
暂时把转化回复的模式的问题搁置，我们可以看到，模式匹配的问题就是一个Lisp函数equal的生成。下面我们展示的函数simple-equal，就类似于内建函数equal，函数pat-match，就是被扩展成模式匹配变量：
(defun simple-equal (x y)
  “Are x and y equal? (Don’t check inside strings.)”
  (if (or (atom x) (atom y))
    (eql x y)
    (and (simple-equal (first x) (first y))
    (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input)
  “Does pattern match input? Any variable can match anything.”
  (if (variable-p pattern)
   T
    (if (or (atom pattern) (atom input))
      (eql pattern input)
      (and (pat-match (first pattern) (first input))
        (pat-match (rest pattern) (reat input))))))

在我们往下走之前，我们需要决定一个模式匹配变量的实现。我们可以，比个例子来说，给定一个符号的集合来作为变量，比如X,Y,Z。是可以定义一个类型的结构variable，但是之后我们每一次想要变量的时候就不得不输入(make-varibale :name ‘X)来进行创建了。另一个选择就是使用符号，但是不指定符号的名字。例如，在Prolog中，变量时从大写字母和小写的常量开始的。但是Common Lisp是大小写敏感的，所以这个方法不适用。但是在基于Lisp的AI程序开发中有一个传统就是，适用问号开头的符号作为变量。
现在为止，我们已经可以处理原子类型的符号，就是那些没有内部结构的对象。但是无论在Lisp中还是在物理世界，事情在第一次出现后总是会变得更加复杂，结果就是即使是原子也开始有组件了。特别是，富豪有名字，字符串可以通过symbol-name函数进行访问。字符串中的元素就是字符，可以已通过函数char访问。问号的表示是通过转义字符来表示。所以断言variable-p可以如下定义，我们现在有了一个完整的模式匹配器：

`(defun variable-p (x)`
`“Is x a variable (a symbol beginning with ‘?’) ?”`
`(and (symbol x) (equal (char (symbol-name x) 0) #\?)))`
`> (pat-match '(I need a ?X) '(I need a vacation)) `
`T `
`¬> (pat-match '(I need a ?X) '(I really need a vacation)) `
`NIL`

每一种情况我们都能得到正确的答案，但是我们没有获得人格关于X是什么的提示，所以就不能在回复中替代什么了。我们需要修改pat-match来返回某种变量和回复的值的表格。在这个选择的当口，有经验的Common Lisp程序员可以用投机取巧的方法来节省时间：找找看现在手边是不是有现成的函数可以完成任务中的发部分工作。我们想要的是在回复中替代变量的值。机警的程序员可能在一些参考书或者本书的索引中找到了函数substitute，subst和sublis。这些函数都是可以用一个表达式来替代旧的表达式的函数。Sublis看下来是最合适的，因为他只允许我们一次性做一些替换。Sublis接受两个参数，第一个是新老的对的列表。第二个是一个进行替换的表达式。对于这个一对中的每一个，car都由cdr来替代，换句话说，我们会把每一对都格式化成类似于(cons old new)的形式。（这样的列表被称为联合列表，a-list，因为他联合了键和值，请见3.6小节），根据上面的例子：
`> (sub 1 is '((?X . vacation))` 
`'(what would it mean to you if you got a ?X ?) `
`(WHAT WOULD IT MEAN TO YOU IF YOU GOT A VACATION ?)`
现在我们要修改pat-match来返回一个列表，而不是在成功的时候仅仅返回T，下面是第一个尝试修改的例子：
(defun pat-match (pattern input)
 “Does pattern match input? WARNING: buggy version.”
  (if (variable-p pattern)
    (list (cons pattern input))
    (if (or (atom pattern) (atom input))
      (eql pattern input)
      (append (pat-match (first pattern) (first input))
        (pat-match (rest pattern) (rest input))))))

这个实现看上去是很合理的：如果模式是一个变量，他会返回一个单元素的a-list如果输入都是列表，那么就会追加在联合列表。然而还是有一些问题，测试语句(eql pattern input)可能会返回T，那不是列表的话，append函数可能会报错的。第二，这个测试也可能会返回nil，也就是表示失败，但是会被认为是一个列表，追加到答案的后面。第三，我们还没有分清楚哪一些是匹配失败，返回nil相对那些匹配一切的情况，但是没有变量。所以他就会返回空的联合列表。（这就是在之前讨论的子断言问题），第四，我们想要变量的绑定统一，如果？X在模式中被两次使用的话，我们不会匹配两个不同的输入的值。最后，对于pat-match，同时检查列表的first和rest是很低效的，甚至当对应的first部分匹配失败的时候（是不是在一个七行的程序中就有5个bug，感到很神奇？）
我们在取得两个主要的共识之后就能分析这些问题了，首先，让pat-match成为一个纯粹的断言会让事情更加方便，所以我们同意在表示失败的时候返回nil，这也就意味着我们需要在一个非nil的值来表示空的绑定列表。第二，如果我们要保持变量值的前后一致性，first就必须知道rest在做设么。我们可以通过传递绑定列表个pat-match的第三个参数来实现。加一个可选的参数，这样就可以简化调用(pat-match a b)。
为了从这些实现决定中抽象，我们定义常量fail和no-bindings来表示两个问题的返回值。特殊形式defconstant被用来显示这些值是不会改变的。（习惯上来说，特殊变量会在名字前后加上星号，但是这个惯例我们在这里不遵循。理由是星号的意思就是引起注意，小心啦，这个变量可能会被词法域之外的某些操作改变值的，但是常量，当然就不会别更改了。）
(defconstant fail nil “Indicates pat-match failure”)
(defconstant no-bindings ‘((t . t))
“Indicates pat-match success, with no variables.”)
接下来我们从assoc函数中抽象出下面四个函数：
(defun get-binding (var bindings)
“Finding a (variable . value) pair in a binding list.”
(assoc var bindings))

(defun binding-val (binding)
“Get the value part of a single binding.”
(cdr binding))

(defun lookup (var bindings)
“Get the value part (for var) from a binding list.”
(binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
“Add a (var . value) pair to a binding list.”
(cons (cons var val) bindings))

现在变量和绑定都定义好了，pat-match就很简单了。他由5部分组成。首先，如果绑定列表是fail，那么匹配失败（因为有些先前的匹配必定是失败了）。如果模式是一个单个的变量，匹配就会返回match-variable所返回的值；可能是一个现有的绑定列表，或者一个扩展，或者是fail。接下来，如果模式和输入都是列表，我们首先在每一个列表的第一个元素递归调用pat-match。这回返回一个绑定列表（或者fail），我们会用来匹配列表的神域部分。这仅仅是一种调用不普通的程序的情况。座椅这种非正式地证明函数会终结的方式是很好的：两个递归调用中的每一个都会降低模式和输入的规模，pat-match会检查原子的模式和输入，所以函数作为一个整体苜蓿返回一个答案（除非模式和输入都是无限的规模）。如果这四个情况没有一种成功，之后匹配失败。

(defun pat-match (pattern input &optional (bindings no-bindings))
 “Match pattern against input in the context of the bindings”
  (cond ((eq bindings fail) fail)
    ((variable-p pattern)
    (match-variable pattern input bindings))
    ((eql pattern input) bindings)
    ((and (consp pattern) (consp input))
    (pat-match (rest pattern) (rest input)
      (pat-match (first pattern) (first input)
       bindings)))
    (t fail))))

(defun match-variable (var input bindings)
  “Does VAR match input? Uses (or updates) and returns bindings.”
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
      ((equal input (binding-val binding)) bindings)
      (t fail))))

现在测试一下pat-match
`> (pat-match '(i need a ?X) '(i need a vacation)) `
`(?X . VACATION) (T . T))`
答案就是绑定在点对的变量的列表；列表的每一个元素都是一个(variable . value)对，(T . T)就是no-bindings的结余。这不是一件坏事，而是可以通过让extend-bindings增加一些复杂性来消除。

(defun extend-bindings (var val bindings)
 “Add a (var . value) pair to a binding list.”
  (cons (cons var val)
   ;;Once we add a “real” binding,
   ;;we can get rid of the dummy no-bindings
    (if (eq bindings no-bindings)
     Nil
     bindings)))

`> (pat-match '(i need a ?X) '(i really need a vacation)) `
`NIL `
`¬ > (pat-match '(this is easy) '(this is easy) ) `
`((T. T)) `
`> (pat-match '(?X is ?X) '((2 + 2) is 4) ) `
`NIL `
`> (pat-match '(?X ;s ?X) '«2 + 2) is (2 + 2))) `
`((?X 2 + 2)) `
`> (pat-match '(?P need. ?X) , (; need a long vacation)) `
`((?X A LONG VACATION) (?P . I))`

请注意nil和((T . T))的区别，后一个意思是匹配成功，但是没有绑定返回。还有，记住(?X 2 + 2) 表示的意思是(? X . (2 + 2))。
Pat-match的更好的实现在第六章会给出。另一个实现在第10.4小节，更加的高效但是也更加臃肿。
