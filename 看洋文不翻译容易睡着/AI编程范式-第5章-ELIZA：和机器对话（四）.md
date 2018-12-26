#####5.4 ELIZA程序：一个基于规则的翻译器
现在我们已经有了一个能用的模式匹配器了，我们还需要一些模式来进行匹配。进一步来说，我们需要模式来与回答进行关联。可以通过引入一个叫做rule的数据结构来做到这件事情，rule由一个模式和一个或者多个关联的回答组成。这些规则的意思就是“如果你看到了A就用B或者C来恢复，随机选择。”我们来实现一个最简单的规则，用列表实现，第一个元素就是模式，剩下的就是一个回复的列表。
(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))
下面是一个规则的例子：
`(((?* ?x) I want (?* ?y))`
`(What would it mean if you got ?y)`
`(Why do you want ?y)`
`(Suppose you got ?y soon))`
接下来我们输入(I want to test this program)，这个规则（当在ELIZA程序内被解释的时候）就会随机选择一个回复，用回复替代变量?y的值，回复(Why do you want to test this program)。
现在我们已经知道了单独的股则是怎么工作的，我们还需要明白如何来处理规则的集合。如果ELIZA是兴趣广泛的，那么就会有很多种不同的回答。所以一些规则可能会用来处理同一个输入。其中一种可能性就是从匹配输入的众多规则中随机选择一个规则。
另一外一种可能就是选择匹配的第一个规则，这表示了规则的集合是一个有序的列表，而不是无序的。聪明的ELIZA规则创建者会利用这种有序，安排一些特定的规则在前面，把某些含糊的规则放在队列的后面。
原始的ELIZA程序有一个优先级系统，每一个规则都有一个确定的优先级数字绑定在上面。优先级最高的匹配规则总是会最先被选择的。请注意，把规则的集合进行有序的排列效果上来说和设置优先级是一样的。第一条规则就是隐式的最高优先级规则，第二条就是次高优先的，以此类推。
这里是一个规则的比较短的列表，从Weizenbaum的论文中节选出来的，但是这里有了规则组织形成的形式了。
`(defparameter *eliza-rules*`
`‘((((?* ?x) hello (?* ?y))`
`(How do you do. Please state your problem.))`
`(((?* ?x) I want (?* ?y))`
`(What would it mean if you got ?y)`
`(Why do you want ?y) (Suppose you got ?y soon))`
`(((?* ?x) if (?* ?y))`
`(Do you really think its likely that ?y) (Do you wish that ?y)`
`(What do you think about ?y) (Really—if ?y))`
`(((?* ?x) no (?* ?y))`
`(Why not?) (You are being a bit negative)`
`(Are you saying “NO” just to be negative?))`
`(((?* ?x) I was (?* ?y))`
`(Were you really?) (Perhaps I already knew you were ?y)`
`(Why do you tell me you were ?y now?))`
`(((?* ?x) I feel (?* ?y))`
`(Do you often feel ?y ?))`
`(((?* ?x) I felt (?* ?y))`
`(What other feelings do you have?))))`

最后我们准备开始定义ELIZA本身。正如我们之前所说，主程序应该是一个不断读取输入的循环，转化输入，输出打印结果。转化过程主要通过找到一些匹配输入的模式规则来完成，之后就会用规则中的回复替换变量。程序在下面的表格中总结。
还有一些小复杂的地方，我们打印一个提示符来告诉用户输入些东西。我们使用函数flatten来确保输出不会在变量替换后嵌入列表。一个重要的技巧就是更改输入，从你到我的转变等等，这些属于也都是相对于说话者，下面是完整的程序：

`(defun eliza ()`
 `“Respond to user input using pattern matching rules.”`
 `(loop`
   `(print ‘eliza>)`
   `(write (flatten (use-eliza-rules (read))) :pretty t)))`

`(defun use-eliza-rules (input)`
 `“Find some rule with which to transform the input.”`
 `(some #’(lambda (rule)`
     `(let ((result (pat-match (rule-pattern rule) input)))`
       `(if (not (eq result fail))`
         `(sublis (switch-viewpoint result)`
           `(random-elt (rule-responses rule))))))`
   `*eliza-rules*))`

`(defun switch-viewpoint (words)`
 `“Change I to you and vice versa, and so on.”`
 `(sublis ‘((I . you) (you . I) (me . you) (am . are))`
   `words))`

函数名|功能含义
---|---
 |顶层函数
Eliza|使用模式匹配规则回复用户的输入
 |特殊变量
Eliza-rules|转换规则的列表
 |数据类型
Rule|一个模式和一系列的回复的组合
 |函数
Eliza|使用模式匹配规则回复用户的输入
Use-eliza-rules|找出可以转化输入的一些规则
Switch-viewpoint|将I更改成you，或者反之，等等
Flatten|将列表中的元素追加到一起
 |用到的Common Lisp函数
Sublis|将元素替换到一颗树中
 |之前定义的函数
Random-elt|从列表中随机选择一个元素
Pat-match|针对输入匹配一个模式
Mappend|将mapcar的结果追加到一起

请注意在write函数中使用的关键字pretty的值是真。这会在某些情况下给出更好的格式化输出。程序利用之气那顶一个random-elt和flatten，他们是使用mappend和mklist来定义的，这个函数在InterLisp方言中定义了，但是没有在Common Lisp中定义。

`(defun flatten (the-list)`
`“Append together elements (or lists) in the list.”`
`(mappend #’mklist the-list))`

`(defun mklist (x)`
 `“Retrun x if it is a list, otherwise (x).”`
 `(if (listp x)`
   `x`
   `(list x)))`

`(defun mappend (fn the-list)`
`“Apply fn to each element of list and append the results.”`
`(apply #’append (mapcar fn the-list)))`

`(defun random-elt (choices)`
`“Choose an element from a list at random.”`
`(elt choices (random (length choices))))`

真实的ELIZA程序要比这里的版本更加复杂一些。首先有一个别名方法来关联一些单词到一些模式上，比如父亲和母亲都会关联到家人这个模式中。也有一个同义词机制来确保，不和否，每人和每个人是一样的意思。由于输入有时候会使用逗号来分割短语，每一个短语都是单独处理的，回复也是使用的最高优先级。另外，原始的ELIZA有一个记忆机制。当没有模式匹配当前输入的时候，但会打印告诉我更多关于X的信息，X就是之前给定的某些输入。最后来说，我们的把呢吧的过则更少，不管我们的版本的短处，他已经可以完成一个对话了，如下：
`> (eliza) `
`ELIZA> (hello there) `
`(HOW DO YOU DO. PLEASE STATE YOUR PROBLEM.) `
`ELIZA> (i want to test this program) `
`(WHAT WOULD IT MEAN IF YOU GOT TO TEST THIS PROGRAM) `
`ELIZA> (i could see if it works) `
`(DO YOU REALLY THINK ITS LIKELY THAT IT WORKS) `
`ELIZA> (no not really) `
`(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) `
`ELIZA> (no) `
`(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) `
`ELIZA> (forget it-- i was wondering how general the program is) `
`(WHY DO YOU TELL ME YOU WERE WONDERING HOW GENERAL THE PROGRAM IS NOW?) `
`ELIZA> (i felt like it) `
`(WHAT OTHER FEELINGS DO YOU HAVE?) `
`ELIZA> (i feel this is enough) `
`(DO YOU OFTEN FEEL THIS IS ENOUGH ?) `
`ELIZA> [Abort] `
`¬>`

最后要说的是，重要的是计数而不是程序，ELIZA已经被解剖地可以请下神坛了。模式匹配仍然是重要的即使，我们会在后面的章节中再见到的。基于规则的翻译器的概念也是很重要的。能够理解英语，或者其他语言的问题领域，仍然是AI中重要的一部分。很明显，理解英语这个问题并没有被ELIZA解决。在第五部分，我们会使用一些更加附加的技术再次讨论这个问题。
#####5.5 历史和参考
如上面所说，描述ELIZA的原始论文是Weizenbaum在1966年写的。另一个使用相似模式匹配技术的对话系统是Kenneth Colby的PARRY。PARRY这个程序模拟的是一个类似具有妄想偏执倾向的人格的人，而且已经像到了可以骗过一些专业心理学家的地步了。虽然模式匹配技术是简单的，但系统维护的意识模型要比ELIZA复杂得多。Colby已经表明了对话程序，如ELIZA，增加了一些意识模型的程序如PARRY，都可以成为在人类精神领域的有用工具。如Colby所说，这些程序可以成为免费而高效的工具，让病人和一个特殊设计程序交互，可以应对一些简单的情况，也可以让魏医生筛选需要更多帮助的病人。其他的有意思的早期对话系统，意识模型是有Allan Collins和Jamie Carbonell提出的。
