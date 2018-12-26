第二部分 早期的AI程序
第四章 GPS：The General Problem Solver
GPS：通用问题解决程序
“现在，世界上有了会思考的机器了”
赫伯特•西蒙
获得诺贝尔奖的AI研究者
这个GPS，是Alan Newell和Herbert Simon在1957年开发的，呈现了一个宏大的图景：给定问题的描述，可以解决任何问题的计算机程序。GPS一经问世就引起了很大的轰动，一些AI研究者也认为人工智能将会迎来一个新的时代。Simon对于这个发明，给以这样的描述：
“不是吓你或者唬你。。。但是我们最简洁的说法就是，现在世界上有了会思考，会学习，会创造的机器。而且，他们的处理能力将会快速增长。直到到有一天，就在可见的未来，他们的可以处理的问题范围和人类理性应用的范围一样宽广。”
虽然GPS从没有实现过那些夸张的宣言，但是他还是历史进程中很重要的一个程序。他是第一个将问题的解决策略从特定问题的知识中分离出来的程序，并且激励了很多在问题解决领域的研究。基于这些理由，他是一个合适的学习对象。
原始的GPS程序有很多增加复杂度的次要特性。另外，GPS是用一种废弃的低级语言IPL写的，这也增加了一些无谓的复杂性。事实上，IPL的疑难的本质可能恰恰是那夸张的宣言的主要理由。我们会忽略一些原始程序的细节部分，使用Common Lisp，比IPL更加明了的语言。结果就是GPS程序变得非常简单，进而阐述一些AI的要点。
从某个角度看，本章要说的就是GPS，但是换个角度，这一章说的也是一个AI程序的开发过程。我们将一个程序的开发过程分为5步。第一个步是问题描述，只是写一个粗糙的想法，一般是用英文写成。第二个步是程序定义，就是用更接近计算过程的方式来重新描述问题。第三步是用编程语言实现程序，比如用Common Lisp，第四步是测试，还有第五步即使调试和分析。这些步骤之间的接线时不固定的，而且步骤的顺序也不是完全固定。在任何一个步骤出现的问题都可能会影响之前的步骤，或者导致程序的重新设计或者项目废弃。程序员会倾向于先完成一部分的描述和定义，然后直接去实现和测试，之后基于一个更好地理解来完成描述。
我们会在GPS的开发过程中遵循着五个步骤，也希望读者会对GPS有更好地理解，之后可以自己独立的写程序。总而言之，AI程序开发的五个步骤如下：
######用模糊术语描述问题
######用算法术语定义问题
######用编程语言实现程序
######用典型用例测试程序
######调试分析程序结果，重复以上步骤
#####4.1 第一步：描述
关于我们的问题描述，我们从Newell和Simon1972年的书Human Problem Solving：这本书中的一段话开始：
>GPS的主要方法体现了目的分析的启发式方法。目的分析的意思用下面的例子来说明：
我想要把儿子送到托儿所去。那在我想送去和我送去了之间的差别是？是距离。怎么改变距离？用汽车。汽车开不了了，不工作。我们怎让汽车工作？新的电池。哪里有新电池？修车铺。我想让修车铺给我一个新的电池，但是修车铺不知道我需要一个新电池。怎么解决？需要通信。怎么通信？电话。。。等等等等。

这种分析归类按照提供的功能和端到端的跳转，需求的函数，还有表现的意思，规定了GPS的启发法的基本规则。
当然，这种分析方式不是完全创新的。这种中间分析最早出现在亚里士多德2300年前的著作，尼各马克伦理学的一章，思考的本质和对象这一章中出现的。
（这里用的是廖申白先生的中译本，详情移步亚里士多德《尼各马可伦理学》第三卷第三节1112b）
>我们所考虑的不是目的，而是朝向目的的实现的东西。医生并不考虑是否要使一个人健康，演说家并不考虑是否要去说服听众，政治家也并不考虑是否要去建立一种法律和秩序，其他的人们所考虑的也并不是他们的目的。他们是先确定一个目的，然后来考虑用什么手段和方式来达到目的。如果只有一种手段，他们考虑的就是怎样利用这一手段去达到目的，这一手段又需要通过哪种手段来获得。这样他们就在所发现的东西中一直追溯到最初的东西，如果恰巧碰到不可能的事情，例如需要钱却得不到钱，那么就放弃这种考虑。而所谓可能的事情，就是我们自身能力可以及达的那些事情。

给定这个问题的解决理论的描述，之后我们应该怎样写这个程序呢？首先我们来更全然的理解这段话的框架。解决问题的主旨就是说要使用一种叫做手段-目的分析法的过程，问题的描述是根据想要发生的事情来描述。在Newell和Simon的例子中，问题就是带小孩去学校，但是一般来说我们会希望程序结局一些更具有普遍意义的问题。如果我们找到一种范式来评估，我做到了额我想做之间的差距的方法，我们就能解决一个问题。举个例子，计入我有一个孩子在家里面，之后我想要孩子到学校，开车是一个解决方法，因为我们知道开车可以让东西的位置发生移动。我们以该注意到，使用手段目的分析方法就是一个选择：可能是从当前的状态开始搜索目的的方式，或者是应用一种不同的搜索策略的混合。
有一些行为需要一些预设条件的解决作为子问题。在我们可以开车之前，我们需要解决的问题是，我们有一辆可以工作的车。也许车子已经准备好了，我们对于这个子问题就不需要做任何事情。所以说一个问题的解决，要么是直接由合适的动作来解决问题，要么就是首先搞定相关的的预设条件子问题，之后采取操作。了然的就是，我们首先需要一些可以采取的行动的描述，并且还有他们的预设条件和效果。然而，如果我们可以把这些概念定义的更好一些，就不需要任何新的概念了。因此，我们会独断地决定问题描述已经完成，之后进入问题定义阶段。
#####4.2 第二步：定义
这里我们会对GPS如何解决问题有一个模糊的概念。我们可以将概念不断精炼，最后成为接近Lisp的形式：
我们可以展示当前世界的状态为：我现有的，或者将目标状态：我想想要的，作为一个条件的集合。Common Lisp是没有结合数据类型的，但是可以用已有的列表来实现集合。灭一个条件都可以用一个符号来表示，因此典型的目标会是一个两个条件的列表(rich famous)，而一个典型的当前状态就是(unknown poor)。
我们需要一个可用的操作符的列表。列表在问题过程中回事不变的，或者就是一系列的问题，但是我们想可以更改列表和处理新的问题领域。
一个操作符可以看做是一个由操作，预设条件和效果组成的结构。通过限定效果的含义就是对当前状态的增减，我们可以给可能的效果增加限制。因此，效果的列表可以分成增加列表和删除列表。这也是strips的GPS实现所采用的方法，也就相当于在效果上重构本章了。Strips是斯坦福的问题解决程序开发的版本。原始的GPS会在效果的定义上有更多的灵活性，但是灵活性会带来低效率。
一个完整的GPS问题被描述成一个起始的状态，一个目标状态和一个已知操作符的集合。因此，GPS会是一个三个参数的函数。例如，下面的例子：
(GPS ‘(unknown poor) ‘(rich famous) list-of-ops)
换言之，一开始的状态是又穷又籍籍无名，可以使用一些已知的操作来获得妇幼且有名的状态。GPS应该只在问题解决的情况下返回真值，并且要打印每一个操作。最简单的方法就是遍历目标状态需要的条件然后一个个尝试去完成，如果条件都达到了，那么问题就解决了。
单个目标条件可以用两种方法达到。如果他已经是在当前状态中，就可以不付出努力达到了。否则，我们就需要找到一个合适的操作符来实现条件。
一个操作符，在效果可以给结果的当前状态有所增加的情况的话，就认为是合适的操作符，也就是说，目标是在操作符的增加列表中的。
如果我们满足了所有的预设条件，皆可以应用操作符了。但是这很简单，因为我们仅仅是在之前的章节中定义了目标的概念。一旦预设条件满足，应用操作符意味着执行操作并且根据操作符的增加列表和删除列表来对当前状态进行更新。既然我么的程序仅仅是一个模拟，他不会真的开一辆车或者打一个电话，我们就必须直白地打印出所有操作，而不是作出真实的行动。
#####4.3 第三步：实现
定义部分可以直接导出一个完全的Common Lisp程序了。下图总结了变量，数据类型，组成GPS程序的函数，还有一些用来完成实现的Common Lisp函数。

名字|解释
---|---
|顶层函数
GPS|使用一系列操作符解决当前状态到目标状态的问题
 |特殊变量
State|当前状态：条件的来列表
Ops|可用的操作符的列表
 |数据类型
Op|一个使用预设条件，增加列表和删除列表的操作符
 |函数
Achieve|获得单个的目标
Appropriate-p|决定一个操作符是不是适合一个目标
Apply-op|操作符应用到当前状态
 |选用的Common Lisp函数
Member|测试是不是列表的一个元素
Set-difference|所有的元素术语一个集合但是不属于另一个
Union|聚合所有两个集合中的元素
Every|测试是不是列表中的每一个元素都可以通过测试
Some|测试是不是列表中的任意一个元素可以通过测试
 |之前定义的函数
Find-all|所有匹配的元素的列表

下面是GPS的源代码：
`(defvar *state* nil “The current state: a list of conditions.”)`

`(defvar *ops* nil “A list of available operators.”)`

`(defstruct op “An operation”`
 `(action nil) (precons nil) (add-list nil) (del-list nil))`

`(defun GPS (*state* goals *ops*)`
 `“General Problem Solver:achieve all goals using *ops*.”`
 `(if (every #’achieve goals) ‘solved))`

`(defun achieve (goal)`
 `“A goal is achieved if it already holds,`
 `Or if there is an appropriate op for it that is applicable.”`
 `(or (member goal *state*)`
   `(some #’apply-op`
     `(find-all goal *ops* :test #appropriate-p))))`

`(defun appropriate-p (goal op)`
 `“An op is appropriate to agoal if it is in its add list.”`
 `(member goal (op-add-list op)))`

`(defun apply-op (op)`
 `“Print a message and update *state* if op is applicable.”`
 `(when (every #’achieve (op-preconds op))`
   `(print (list ‘executing (op-action op)))`
   `(setf *state* (set-difference *state* (op-del-list op)))`
   `(setf *state* (union *state* (op-add-list op)))`
   `t))`

这个代码是由7个定义组成的。这些定义对应于上面描述中的7个项目。一般来说，你不应该期望在实现和设计之间有这种完整的对应。有两个defvar形式，一个defstruct和四个defun。这些都是Common Lisp用来定义变量，结构和函数的形式。他们是Lisp中最通用的顶层形式，但是他们没有什么特殊的魔力，仅仅是给Lisp环境加上一些自定义的变量的有特殊副作用的特殊形式而已。
在下面重复出现的两个defvar形式声明了特殊变量state和ops，之后我们可以在程序的任何地方访问他们。
`(defvar *state* nil “The current state: a list of conditions.”)`
`(defvar *ops* nil “A list of available operators.”)`
Defstruct定义了一个叫做op的结构，这个结构有几个位置组成，action，preconds，add-list和del-list。Common Lisp中的结构和C中的结构或者Pascal中的记录很相似。Defstruct会自动定义一个构造器函数，叫做make-op，还对每一个位置都生成一个访问函数。访问函数叫做，op-action，op-preconds，op-add-list和op-del-list。Defstruct也定义了赋值函数，copy-op，断言，op-p还有setf用来更改位置的。这些函数不会用在GPS中，概略地看，defstruct会展开成下面的样子：
(defstruct op “An operation”
(action nil) (preconds nil) (add-list nil) (del-list nil))
会展开成下面的定义：
(defun make-op (&key action preconds add-list del-list)
(vector ‘op action preconds add-list del-list))
(defun op-action (op) (elt op 1))
(defun op-preconds (op) (elt op 2))
(defun op-add-list (op) (elt op 3))
(defun op-del-list (op) (elt op 4))
(defun copy-op (op) (copy-seq op))
(defun op-p (op)
(and (vector op) (eq (elt op 0) ‘op)))
(setf (documentation ‘op ‘structure) “An operation”)
后面是GPS程序的四个函数定义。主函数，GPS，会接受三个参数。第一个就是当前世界的状态，第二个是目标状态，第三个是一个可选操作符的列表。函数的主体，简单来说就是如果我们可以获得给定的每一个目标状态，那么问题就解决了。另外不必说的就是，如果不满足，问题就没解决。
函数achieve的参数是一个单个的目标。接下来会检测，这个目标是不是在当前状态就满足（这样就不用采取任何操作）或者可以通过一些可用操作实现。首先是建立一个可用操作的列表之后挨个测试，直到有一个是可用的为止。Achieve会调用find-all，find-all会根据断言appropriate-p来返回一个与当前状态匹配的操作符列表。
函数apropriate-p的作用是测试一个操作符是不是对达成目标是合适的（这个函数依照Lisp命名惯例，结尾是断言p）。
最后，函数apply-op，检测我们是不是可以获取所有这个操作符的预设条件之后应用操作符。这个应用包括一些行为，打印信息，通过删除del-list的元素和增加add-list的元素更改现实状态。Apply-op也是一个断言，只在操作符可以应用的时候返回t。
#####4.4 第四步：测试
本节会定义乙烯类可应用的操作符来因对带孩子去学校这个问题，进而展示和解决这个问题。首先，我们需要为问题构造一个操作符的列表。Defstruct形式为op自动定义了函数make-op，可以这么用：
(make-op :action ‘drive-son-to-school
 :preconds ‘(son-at-home car-works)
 :add-list ‘(son-at-school)
 :del-list ‘(son-at-home))
表达式返回是一个操作符，action位置是符号drive-son-to-school，预设条件是add-list和del-list都是特定的列表。操作符的目的是，无论何时孩子在家里和汽车能开，drive-son-to-school都是可以使用的，通过删除孩子在家或者增减孩子在学校来更改现实状态。
以该注意的是使用连字符连接的原子，如aon-at-home仅仅是对那些比较简单的例子有用。更好的办法是将原子打碎成组件：也许是(at son home)，使用基于原子的方法的问题是众多组合的一个。如果有10个断言，比如说at，还有10个人或者对象，那么就有1000种可能的连字符原子组合，但是组件只有20个。本章中我们会使用连字符的原子因为他更加简单，我们就不需要描述整个世界了。后面的章节我们会展示的更加严谨一些。
这个操作符是一个模型，根据前面两位作者的定义，我们可以定义其他操作符。会有一个函数安装电池，告诉修车铺问题，打电话给商店。我们可以增加查找电话的操作符和给钱付款的操作符。
(defparameter *school-ops*
  (list
    (make-op :action ‘drive-son-to-school
     :preconds ‘(son-at-home car-works)
     :add-list ‘(son-at-school)
     :del-list ‘(son-at-home))
    (make-op :action ‘shop-installs-battery
     :preconds ‘(car-needs-battery shop-knows-problem shop-has-money)
     :add-list ‘(car-works))
    (make-op :action ‘tell-shop-problem
     :preconds ‘(in-communication-with-shop)
     :add-list ‘(shop-knows-problem))
    (make-op :action ‘telephone-shop
     :preconds ‘(know-phone-number)
     :add-list ‘(in-communication-with-shop))
    (make-op :action ‘look-up-number
     :preconds ‘(have-phone-book)
     :add-list ‘(know-phone-number))
    (make-op :action ‘give-shop-money
     :preconds ‘(have-money)
     :add-list ‘(shop-has-money)
    :del-list ‘(have-money))))
下一步就是给GPS掰出问题来测试解决方案。后面是三个典型问题。每一种情况下，目标都是相同的，就是为了获得条件son-at-school。可用的操作符列表在没一个问题中也是相同的；不同的是初始状态。三个例子都是用尖括号开头，也就是Lisp系统的提示符，后面是GPS的调用，之后由程序打印信息，最后是程序的结果，solved或者nil。
`> (gps '(son-at-home car-needs-battery have-money have-phone-book) `
`'(son-at-school) `
`*school-ops*) `
`(EXECUTING LOOK-UP-NUMBER) `
`(EXECUTING TELEPHONE-SHOP) `
`(EXECUTING TELL-SHOP-PROBLEM) `
`(EXECUTING GIVE-SHOP-MONEY) `
`(EXECUTING SHOP-INSTALLS-BATTERY) `
`(EXECUTING DRIVE-SON-TO-SCHOOL) `
`SOLVED`

`> (gps '(son-at-home car-needs-battery have-money) `
`‘(son-at-school) `
`*school-ops*) `
`¬ NIL`

`> (gps '(son-at-home car-works) `
`‘(son-at-school) `
`*school-ops*) `
`(EXECUTING DRIVE-SON-TO-SCHOOL) `
`SOLVED`
三个例子的目标都是要把孩子送到学校。在add-list中唯一一个可以让目标实现的操作符就是drive-son-at-school，所以GPS一开始选择了这个操作符。在这个操作符可移植性之前，GPS必须要解决先决条件。第一个例子中的整个工作流程回溯之后是这样，shop-intalls-battery，give-shop-money，tell-shop-problem和telephone-shop，look-up-number，由于没有先决条件，所以look-up-number可以直接执行，然后一步步执行其他操作。正如亚里士多德说的那样：分析链条的最后一步恰恰就是执行的第一步。
第二个例子的初始状态和第一个是一样的，但是执行look-up-numer操作符却失败了，因为它的先决条件have-phone-book没有被满足。所以GPS无事可做，返回nil。
最后，第三个例子就更加直接了，初始状态定义了，车子是可以开的，所以驾车操作可以马上应用了。
#####4.5 分析，或者说，我们关于GPS说谎了
在下面的章节中，我们检查到底我们的GPS版本是有多么通用。下面四个章节都是支出我们的GPS版本的局限的。我们会在程序的第二个版本中修正这些局限。
有人也许会问了，局限是不是就是bug的委婉说法。我们是在增强程序还说我们是在矫正程序？关于这点还咩有明确的答案，因为我们不可能永远坚持一个绝对清楚的问题描述和定义。AI编程很大程度上是一种探究的编程；主要目的经常是为了研究更多的问题领域，而不是为了下一个明确的定义。这种说法相对常见于传统编程中，就是问题在第一行代码写下之前一定要说清楚。
#####4.6 转圈儿跑问题
操作符，带孩子去学校这种行为是很好展现的：减去孩子在家，加上孩子在学校就OK了。但是如果我们想要展现转圈儿跑这种行为，也就意味没有位置的移动，那是不是没有了add-list和del-list？如果真是这样，是不是就没有了应用操作符的意义。也许add-list本身就该包含比如，做点运动或者感到累了这样的内容，或者更加具体的，跑圈锻炼一下这样的描述。我们之后再来讨论这个问题。
#####4.7 兄弟目标冲突问题
过日子不仅仅是要把孩子送到学校，还要剩下钱贴补家用不是。GPS可以再下面的初始条件下解决问题：
`> (gps '(son-at-home have-money car-works) `
`'(have-money son-at-school) `
`*school-ops*) `
`(EXECUTING DRIVE-SON-TO-SCHOOL) `
`SOLVED`
但是在下面的例子中，GPS返回的问题解决了是有问题的，事实上钱这个条件我们已经用过了。
`> (gps '(son-at-home car-needs-battery have-money have-phone-book) `
`'(have-money son-at-school) `
`*school-ops*) `
`(EXECUTING LOOK-UP-NUMBER) `
`¬EXECUTING TELEPHONE-SHOP) `
`(EXECUTING TELL-SHOP-PROBLEM) `
`(EXECUTING GIVE-SHOP-MONEY) `
`(EXECUTING SHOP-INSTALLS-BATTERY) `
`(EXECUTING DRIVE-SON-TO-SCHOOL) `
`SOLVED`
问题出在GPS会使用表达式(every #’achieve goals)来大城一系列目标。如果表达式返回true，意思就是序列中的每一个目标都完成了，但这并不意味了他们他最后都是为真的！换句话说，我们目标(have-money son-at-school)，我们的理解是，我们最后是既有钱而且娃也在学校了，但是GPS的理解是首先是有钱了，OK，之后是娃在学校，OK，就返回成功了。有时候，完成一个目标可以吧另一个之前已经大城目标给冲突抹杀掉，我们称这种现象是兄弟目标冲突问题。这个冲突在于，have-money和son-at-school是一对兄弟目标，为了实现son-at-school这个目标的前提就是car-works，为了达成这个目标就要把已经OK的目标have-money给冲突干掉。
修改程序，识别出兄弟目标冲突是很简单明了的。首先请注意我们在程序中两次调用(every #’achieve something)，之后，我们就是用(achieve-all something)来代替这两个形式，可以这么定义achieve-all：
`(defun achieve-all (goals)`
 `“Try to achieve each goal, then make sure they still hold.”`
 `(and (every #’achieve goals) (subset goals *state*)))`
Common Lisp函数subsetp就是在第一个参数是第二个参数子集的情况下返回真。在achieve-all中，如果最后的目标状态仍然是状态的子集的话就会返回真，这就是我们要测试的。
Achieve-all的引入防止了GPS在目标冲突的情况下返回真，但是他不会强迫GPS重新计算。我们现在不考虑这种可能性，斗牛士后面我们会再研究。
#####4.8 跳悬崖之前先看看
另一种观点看兄弟目标冲突问题就是仅仅把目标列表中的目标顺序做调整。如果我们想要带孩子去学校，还想要有钱能剩下，为什么不能定义(son-at-school have-money)呢？我们来看看会发生什么？
`> (gps '(son-at-home car-needs-battery have-money have-phone-book) `
`'(son-at-school have-money) `
`*school-ops*) `
`(EXECUTING LOOK-UP-NUMBER) `
`(EXECUTING TELEPHONE-SHOP) `
`(EXECUTING TELL-SHOP-PROBLEM) `
`(EXECUTING GIVE-SHOP-MONEY) `
`(EXECUTING SHOP-INSTALLS-BATTERY) `
`(EXECUTING DRIVE-SON-TO-SCHOOL) `
`NIL`
GPS返回nil的意思就是目标没有被完成，仅仅只是执行了到带孩子去学校这里的操作。我们家这种问题是跳之前先看看问题，假设你要求系统达成两个目标，跳下悬崖和安全落地，首先你很高兴地跳下了悬崖，一个目标达成，之后你发现没有操作符满足安全落地这个目标。很明显，这个程序执行得很不谨慎。哈哈
问题在于，这里得计划和执行是脱节的。一旦一个操作符的先决条件满足，操作符就会执行，state状态就会不可逆地改变，甚至说程序会导致严重后果也是。改变的方法就是说，我们用一些本地的状态变量来替换全局的变量state，每一个状态都睡创建一个新的变量来对应。这种更改带来的好处我们在下一小节看看。
在我们虚拟的托儿所世界里面，只有一种方法去找电话号码，假设我们想要加一种找电话号码的方式，比如询问别人，这个操作符。当然，因为要问事情，你需要是可以跟他沟通的。操作符asking-for-a-phone-number可以这么实现：
`(push (make-op :action 'ask-phone-number `
   `:preconds '(in-communication-with-shop) `
   `:add-list '(know-phone-number)) `
 `*school-ops*)`
（特护形式(push item list)会把一个项目加到一个列表的前面；等同于(setf list (cons item list))的简单形式。）不幸的是，看上去这么简单的问题，使用这种方法处理的时候会出现一些意想不到的问题。
`> (gps '(son-at-home car-needs-battery have-money) `
`'(son-at-school) `
`*school-ops*) `
`¬>>TRAP 14877 (SYSTEM:PDL-OVERFLOW EH::REGULAR) `
`The regular push-down list has overflown. `
`While in the function ACHIEVE <- EVERY <- REMOVE`
错误信息的意思是出现了太多的递归嵌套函数调用。这表示一个很复杂的问题或者更加普遍的程序bug导致了无限循环。查看bug原因的一个方式就是追踪相关的函数，比如achieve：
`> (trace achieve) =? (ACHIEVE) `
`¬> (gps '(son-at-home car-needs-battery have-money) `
`'(son-at-school) `
`*school-ops*) `
`(1 ENTER ACHIEVE: SON-AT-SCHOOL) `
`(2 ENTER ACHIEVE: SON-AT-HOME) `
`(2 EXIT ACHIEVE: (SON-AT-HOME CAR-NEEDS-BATTERY HAVE-MONEY)) `
`(2 ENTER ACHIEVE: CAR-WORKS) `
`(3 ENTER ACHIEVE: CAR-NEEDS-BATTERY) `
`(3 EXIT ACHIEVE: (CAR-NEEDS-BATTERY HAVE-MONEY)) `
`(3 ENTER ACHIEVE: SHOP-KNOWS-PROBLEM) `
`(4 ENTER ACHIEVE: IN-COMMUNICATION-WITH-SHOP) `
`(5 ENTER ACHIEVE: KNOW-PHONE-NUMBER) `
`(6 ENTER ACHIEVE: IN-COMMUNICATION-WITH-SHOP) `
`(7 ENTER ACHIEVE: KNOW-PHONE-NUMBER) `
`(8 ENTER ACHIEVE: IN-COMMUNICATION-WITH-SHOP) `
`(9 ENTER ACHIEVE: KNOW-PHONE-NUMBER)`
`…`
Trace的输出给了我们必要的线索。Newell和Simon说过：结果之间的周期循环，要求的功能，还有表现他们的方式。这里好像出现了结果之间的周期震荡，就在和shop联系和寻找shop的电话号码之间，具体的理由是这样，我们想要商店的了解我们的电池的问题，这就要求我们去联系。联系的方式之一是用电话，但是我们没有电话本来查找电话，所以我们就问他们电话，但是这就要求和他们联系。如亚里士多德所说“如果我们一直只是思考，我们就会陷入无限的空洞中。”我们成这个问题是递归字母表问题：尝试去解决依赖与自身的问题。解决的方式是让achieve保持对所有目标工作状态的关注并且在出现无限循环的时候放弃。
#####4.10 中间信息的缺失问题
当GPS没能找到解决办法，他只是返回NIL。这对于想要求解的用户来说是很烦人的，因为他没有给出任何为什么会失败的原因。当然用户可以像上面追踪achieve那样追踪函数，但是trace的输出总是会带有很多无关的信息。如果能有一个通用的调试工具，可以根据程序员的需要在代码中插入信息输出，选择性打印就太好了。
函数dbg就提供了这项功能。Dbg打印输出的方式和format一样，但是仅仅是在调试输出需要的时候打印。每一次对dbg的调用都伴有用来定义一类调试信息的标识符。函数debug和undebug是用来增加或者移除应该打印的调试类别中的类别数目的。在本章，所有的调试信息输出都会使用标识符：gps。其他的程序会使用其他标识符，复杂的程序会使用很多标识符。
一次dbg的调用如果第一个参数是定义在debug调用中的标识符，就会导致输出。Dbg的其他参数就是一个格式化字符串和相应的参数。也就是说，我们写的包括调用dbg的函数是这样的：
(dbg :gps “The current goal is : ~a” goal)
如果我们已经使用表达式(debug :gps)打开了调试，之后对dbg带有标识符：gps的调用就会打印输出。也可以使用表达式(undebug :gps)啦关闭输出。
Debug和undebug的设计很像trace和untrace，用来打开或者关闭调试信息。他们也遵循惯例，debug调用是没有参数的话就会返回当前标识符的列表，undebug没有参数的话就会关闭调试。然而他们和trace，untrace的区别在于他们是函数，不是宏。如果你是用关键字和整数作为标识符，你不会注意到有什么区别。
我想在这里引入两个新的内建特性。首先debug-io会被用做一般调试输入输出的流。在所有之前对format的调用中，我们使用t作为流参数，将输出导向标准输出流。发送不同类型的输出给不同的流会赋予用户灵活性。例如，调试输出可以被导向到一个单独的窗口，或者可以被拷贝到一个文件中。第二，函数fresh-line会达到输出的下一行，除非输出流已经是在行的头了。
`(defvar *dbg-ids* nil "Identifiers used by dbg")`

`(defun dbg (id format-string &rest args) `
 `"Print debugging info if (DEBUG ID) has been specified." `
 `(when (member id *dbg-ids*) `
   `(fresh-line *debug-io*) `
   `(apply #'format *debug-io* format-string args)))`

`(defun debug (&rest ids) `
 `"Start dbg output on the given ids." `
 `(setf *dbg-ids* (union ids *dbg-ids*)))`
`(defun undebug (&rest ids) `

 `"Stop dbg on the ids. With no ids, stop dbgaltogether." `
 `(setf *dbg-ids* (if (null ids) nil `
   `(set-difference *dbg-ids* ids))))`
有时候给调试输出根据模式加上缩进会更加容易看，比如函数嵌套调用的深度。为了生成缩进的输出，可以定义函数dbg-indent：
`(defun dbg-indent (id indent format-string &rest args) `
 `"Print indented debugging info if (DEBUG ID) has been specified." `
 `(when (member id *dbg-ids*) `
   `(fresh-line *debug-io*) `
   `(dotimes (i indent) (princ" "*debug-io*)) `
   `(apply #'format *debug-io* format-string args)))`

