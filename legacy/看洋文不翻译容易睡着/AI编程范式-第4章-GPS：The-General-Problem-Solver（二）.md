#####4.11 GPS版本2：一个更加通用的GPS
这里这个版本的GPS中，我们会解决跑圈儿问题，兄弟目标冲突问题，跳之前看看，和递归子目标问题。下图是新版本的术语表。

名字|功能含义
---|---
 |顶层函数
GPS|从一个状态中使用一系列操作符解决目标
 |特殊变量
Ops|可用的操作符列表
 |数据类型
Op|带有前提条件和add-list，del-list的操作
 |主要函数
Achieve-all|完成一个目标的列表
Achieve|完成一个单个的目标
Appropriate-p|决定一个操作符是不是适合一个目标
Apply-op|把一个操作符应用到当前状态
 |辅助函数
Executing-p|判断一个条件是不是一个executing形式
Starts-with|判断一个参数是不是以给定原子开头的列表
Convert-op|使用executing惯例来转化一个操作符
Op|创建一个操作符
Use|使用一个操作符的列表
Member-equal|特使是不是一个元素等于一个列表中的成员元素
 |使用的Common Lisp函数
Member|测试一个元素是不是一个列表的成员
Set-difference|数以一个集合的元素，但是不属于另一个
Subsetp|是不是一个集合完全包含在另一个中
Union|两个集合中所有的元素
Every|测试列表中的元素是不是都能通过测试
Some|测试列表中的任意一个元素能够通过测试
Remove-if|删除所有满足条件的项目
 |之前定义过的函数
Find-all|所有匹配的元素的一个列表
Fin-all-if|所有匹配断言的元素的一个列表

其实最重要的变化是这个，我们不让GPS返回每一个操作符返回的信息打印，而是让GPS返回结果的状态。每一个状态的信息的列表都显示做了那些操作。每一个信息实际上都是一个条件，一个个形式(executing 操作符)的列表。这解决了绕圈儿跑的问题：我们可以以初始状态((executing run-around-block))来调用GPS，太久会执行操作符run-around-block。由此满足了目标。下面的代码定义了一个新的函数，op，会创建一个操作符，在他们的add-list中包含了信息。
(defun executing-p (x)
  “Is x of the form: (executing …) ?”
 (starts-with x ‘executing))

(defun starts-with (list x)
  “Is this a list whose first element is x ?”
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  “Make op conform to the (EXECUTING op) convertion.”
  (unless (some #’executing-p (op-add-list op))
    (push (list ‘executing (op-action op)) (op-add-list op)))
 op)

(defun op (action &key preconds add-list del-list)
  “Make a new operator that obeys the (EXECUTING op) convertion.”
  (convert-op
    (make-op :action action :preconds preconds
     :add-list add-list :del-list del-list)))

由op构建的操作符是正确的，但是我们可以把现有的操作符通过convert-op直接转化：
`(mapc #’convert-op *school-ops*)`
只是一个探索编程技术前沿的例子，在发现软件现有版本的确信啊的时候，不是重新构造一个，而是使用Lisp来更改现有的数据结构来适应程序的新版本。
变量ops的定义，结构op的定义和他们之前的定义是完全相同的，程序剩下的部分是我们之前见过的五个函数组成：GPS，achieve-all，achieve，appropriate-p和apply-op。在顶层，函数GPS调用achieve-all，会返回nil或者一个合法的状态。这个中，我们会移除所有的原子，只留下最终状态的列表中的元素，其实也就是形式(executing operator)的行为。因此，GPS的值本身就是一个达到最终状态的行为的一个列表。GPS在找到解决方法的时候不再返回SOLVED，而是遵守惯例，返回nil作为失败，非nil作为成功。一般来说，让程序返回一个有意义的值而不是打印那个值，是一个好的做法，这样子其他程序或许就会使用那些返回的值。
`(defun *ops* nil “A list of available operators.”)`

`(defstruct op “An operation”`
 `(action nil) (preconds nil) (addlist nil) (del-list nil))`

`(defun GPS (state goals &optional (*ops* ** ops))`
 `“General Problem Solver: from state, achieve goals using *ops*.”`
 `(remove-if #’atom (achieve-all (cons ‘(start) state) goals nil)))`

新版本的第一个明显变化就是程序的第一行：没有了state变量。程序会对本地的状态变量进行追踪。为了解决跳之前先看看问题，这就是这个目的。函数achieve，achieve-all，和apply-op都会接受一个当前状态的额外的参数，并且都会返回一个新的状态作为他们的值。他们也必须遵循惯例，在失败的时候返回nil。
因此我们有了一个潜在的模糊概念：是不是nil代表了失败？或者nil代表的是没有条件的合法值？我们辨明这个模糊概念的方法是，适应惯例，所有的状态都必须有至少一个条件，这个惯例是由函数GPS强制的。不再调用(achieve-all state goals nil)，GPS调用(achieve-all (cons ‘(start) state) goals nil)。所以即使用户给GPS传入的是NULL的初始状态，其实也会传进一个包含了(state)的状态给achieve-all。这样子的话，我们就保证了没有状态将会是nil的，因为唯一能构造一个新状态的函数是apply-op，我们可以看到在apply-op的最后一行总是会在返回状态后面加上一些东西的。（一个add-list永远不会是nil，因为如果他是，操作符就不会合适，另外，每一个操作符都包含(executing …)条件。）
请注意我们最终从GPS中返回的值是有所有的删除的原子的，所以我们结束的仅仅是执行的操作的报告，他们是由条件形式(executing action)来展现的。把条件(start)加到开始的前面也表示问题不能解决或者问题不需要操作就能解决之间的区别。失败就会返回nil，一个灭幼步骤的解决方案至少会包含条件(start)，即使没有其他的。
返回nil作为失败，但会其他一些有用的值的函数被称作半断言。在nil仅仅是被翻译成一个有用的值的情况下，会导致一些错误发生的倾向。第一，决定nil是不是会是一个有意义的值。第二，保证用户不能把nil最为一个值来破坏程序。在本程序中，GPS是用户唯一调用的程序，只要搞定这个就可以了。第三，保证程序不能把nil作为一个值。我们确保在程序中只有一个地方可以构造程序状态，新状态的形成是通过在另一个状态之后追加一个氮元素列表。通过这三步过程，我们有了一个非正式的论据，子断言包含的状态会正确运行。这种非正式的论证过程是一个好的程序设计的一般元素。
新版本的另一个大改变就是一个目标栈结构的引入来解决子目标问题。程序会追踪目标的完成过程，如果子目标就是目标本身的话，马上就会返回失败。这个测试在achieve的第二个语句中进行。
函数achieve-all会尝试完成目标中的每一个，设置变量state2作为每一个成功调用achieve的返回值。如果所有的目标都一次完成，如果所有的目标在最后都在的话（subsetp来检测），之后就会返回最终的结果状态。否则函数就会失败，返回nil。
大部分的工作是由achieve完成的，接受一个状态的输入，一个结果条件还有目标栈。如果条件已经在状态中的话，之后achieve成功并返回值。另一方面，如果目标的条件已经在目标栈中的话，之后就没有必要再继续了，我们会陷入无限循环，所以就返回nil。否则，achieve检索操作符的列表，尝试找到一个匹配的操作符。

`(defun achieve-all (state goals goal-stack)`
 `“Achieve each goal, and make sure they still hold at the end.”`
 `(let ((current-state state))`
   `(if (and (every #’(lambda (g)`
           `(setf current-state`
             `(achieve current-state g goal-stack)))`
         `goals)`
       `(subset goals current-state :test #’equal))`
     `Current-state)))`

`(defun achieve (state goal goal-stack)`
 `“A goal is achieved if it already holds,`
 `Or if there is an appropriate op for it that is applicable.”`
 `(dbg-indent :gps (length goal-stack) “Goal: ~a” goal)`
 `(cond ((member-equal goal state) state)`
   `((member-equal goal goal-stack) nil)`
   `(t (some #’(lambda (op) (apply-op state goal op goal-stack))`
     `(find-all goal *ops* :test #’appropriate-p)))))`

目标((executing run-around-block))是一个单个条件的列表，条件可能会是一个双元素的列表。允许列表作为条件使得程序更加有灵活性，但是我们也不得不要小心。问题在于不是所有的列表看上去像就是完全一样的。断言equal基本的测试去看是不是他的两个参数看上去很像。断言eql测试的是两个参数是不是实际上一样的。既然函数member一类的函数是默认用eql的，我们不得不行一个test关键字来使用equal。如果使用设计广泛，我们会考虑重新定义一个函数member-equal。事实上，我们本可以进一步抽象定义一个函数member-situation，一个函数用来测试条件在一个环境下是不是为真。这回允许用户更改匹配函数，从eql到equal，之后会很有用。

`(defun member-equal (item list)`
 `(member item list :test #’equal))`

函数apply-op，用来更改状态，而且不可以撤销，打印信息反映状态，现在是返回状态而不是打印信息。首先计算获取所有操作符的预设条件的状态，如果达到这样一个状态是可能的话，之后apply-op返回一个新的状态，通过给add-list加上什么和给del-list删除什么。

`(defun apply-op (state goal op goal-stack)`
 `“Return a new, transformed state if op is applicable.”`
 `(dbg-indent :gps (length goal-stack) ”Consider: ~a” (op-action op))`
 `(let ((state2 (achieve-all state (op-preconds op)`
           `(cons goal goal-stack))))`
   `(unless (null state2)`
     `;;Return an updated state`
     `(dbg-indent :gps (length goal-stack) “Action: ~a” (op-action op))`
     `(append (remove-if #’(lambda (x)`
           `(member-equal x (op-del-list op)))`
         `State2)`
       `(op-add-list op)))))`

`(defun appropriate-p (goal op)`
 `“An op is appropriate to a goal if it is in its add-list.”`
 `(member-equal goal (op-add-list op)))`

在我们计算新状态的道路上，这是最后一个复杂的地方。在之前的GPS版本中，状态是（概念上来说）是无序的条件集合，所以我们可以使用union和set-difference来操作他们。在第二个版本中，状态成为了一个有序的集合，因为我们需要保存动作的顺序。因此，我们不得不使用函数append和remove-if，这些函数都是保存顺序的，但是union和set-difference是不保存顺序的。
最后，版本2的区别在于他引入了一个虚拟的函数：use，这个函数是被用在声明的排序，给定一系列问题的操作符的列表。

`(defun use (oplist)`
 `“Use oplist as the default list of operators.”`
 `;;Return something useful, but not too verbose:`
 `;;the number of operators.`
 `(length (setf *ops* oplist)))`

调用use来设置参数ops，这样就不需要在每次对GPS的调用中指定了。也就是说GPS的第三个参数的定义，ops，现在是可选的了。如果没有提供的话，就会使用默认的。Ops的默认值就是ops。看上去这一部是多余的或者是非常不必要的，一个变量怎么会是自己的默认值呢？问题处在两个ops看上去是一样的，但是他们指向的是两个完全不同的绑定的特殊变量。大多时间，参数列表中的变量是本地变量，但是也没有规定特殊变量不能绑定到参数上。请记住，绑定一个特殊变量的下过是所有在程序中指向特殊变量的引用，甚至是在函数词法域之外，都指向新绑定的特殊变量。所以在一系列调用之后我们到达了achieve，指向的是ops，他会指向新绑定的ops的值。
GPS的定义在这里是重复的，随之的是一个可选的绑定了一个本地那边利息那个和显式设置重置特殊变量ops。很明显，绑定特殊变量的术语是更加精确了，一开始也比较难理解，明白之后就会很好用了。

`(defun GPS (state goals &optional (*ops* *ops*))`
 `“General Problem Solver: from state, achieve goals using *ops*.”`
 `(remove-if #’atom (achieve-all (cons ‘(start) state) goals nil)))`

`(defun GPS (state goals &optional (ops *ops*))`
 `“General Problem Solver: from state, achieve goals using *ops*.”`
 `(let ((old-ops *ops*))`
   `(setf *ops* ops)`
   `(let ((result (remove-if #’atom (achieve-all`
       `(cons ‘(start) state)`
       `Goals nil))))`
     `(setf *ops* old-ops)`
     `result)))`

我们现在来看看版本2是如何运作的，我们使用操作符的列表，包含的而是操作符，问问商店的电话。首先我们确保版本2包含了版本1的所有功能：
`> (use *school-ops*) => 7`
`> (gps '(son-at-home car-needs-battery have-money have-phone-book) `
`'(son-at-school )) `
`((START) `
`(EXECUTING LOOK-UP-NUMBER) `
`(EXECUTING TELEPHONE-SHOP) `
`(EXECUTING TELL-SHOP-PROBLEM) `
`(EXECUTING GIVE-SHOP-MONEY) `
`(EXECUTING SHOP-INSTALLS-BATTERY) `
`(EXECUTING DRIVE-SON-TO-SCHOOL))`
`> (debug :gps) => (:GPS)`
`> (gps '(son-at-home car-needs-battery have-money have-phone-book) `
`'(son-at-school) ) `
`Goal: SON-AT-SCHOOL `
`Consider: DRIVE-SON-TO-SCHOOL `
`Goal: SON-AT-HOME `
`Goal: CAR-WORKS `
`Consider: SHOP-INSTALLS-BATTERY `
`Goal: CAR-NEEDS-BATTERY `
`Goal: SHOP-KNOWS-PROBLEM `
`Consider: TELL-SHOP-PROBLEM `
`Goal: IN-COMMUNICATION-WITH-SHOP `
`Consider: TELEPHONE-SHOP `
`Goal: KNOW-PHONE-NUMBER `
`Consider: ASK-PHONE-NUMBER `
`Goal: IN-COMMUNICATION-WITH-SHOP `
`Consider: LOOK-UP-NUMBER `
`Goal: HAVE-PHONE-BOOK `
`Action: LOOK-UP-NUMBER `
`Action: TELEPHONE-SHOP `
`Action: TELL-SHOP-PROBLEM `
`Goal: SHOP-HAS-MONEY `
`Consider: GIVE-SHOP-MONEY `
`Goal: HAVE-MONEY `
`Action: GIVE-SHOP-MONEY `
`Action: SHOP-INSTALLS-BATTERY `
`Action: DRIVE-SON-TO-SCHOOL `
`( START) `
`(EXECUTING LOOK-UP-NUMBER) `
`(EXECUTING TELEPHONE-SHOP) `
`(EXECUTING TELL-SHOP-PROBLEM) `
`(EXECUTING GIVE-SHOP-MONEY) `
`(EXECUTING SHOP-INSTALLS-BATTERY) `
`(EXECUTING DRIVE-SON-TO-SCHOOL))`
`> (undebug) => NIL`
`> (gps '(son-at-home car-works) `
`'(son-at-school )) `
`((START) `
`(EXECUTING DRIVE-SON-TO-SCHOOL))`

现在我们看看版本2能处理，但是版本1会出错的情况，每一种情况，程序都会避免无限循环，也会防止跳之前看看问题。
`> (gps '(son-at-home car-needs-battery have-money have-phone-book) `
`'(have-money son-at-school)) `
`¬ NIL`

`> (gps '(son-at-home car-needs-battery have-money have-phone-book) `
`'(son-at-school have-money)) `
`¬ NIL`

`> (gps '(son-at-home car-needs-battery have-money) `
`, (son-at-school)) `
`¬ NIL`

最后我们会看到，GPS的版本在对待没有意义的问题上，就不做任何操作：
`> (gps '(son-at-home) '(son-at-home)) => ((START))`
