#####5.3 段模式匹配
在模式(?P need . ?X)中，变量？X匹配的是与长度无关的输入列表的剩余部分。相对的？P也就是只能匹配耽搁元素，就是输入列表的第一个元素了。对于很多模式匹配的应用来说，这很好，我们只想匹配对应的元素。然而，ELIZA在一些地方有些不同，我们需要变量在匹配输入的序列元素的任何地方占位。我们会调用，段变量。我们需要标记来对段变量和普通变量进行区别。主要是有两种方式：要么我们使用原子来表示段变量来通脱拼写惯例来区分（就像哦我们区分变量和常量一样的做法），或者我们使用非原子的构造。我们之后再选择，先使用一个形式(?星号 变量)的列表来表示段变量。选择符号问号星号的原因是他组合了使用克莱尼星号的变量的标记法。所以我们想要的pat-match的行为是这样：
`> (pat-match '((?* ?p) need (?* ?x)) `
`'(Mr Hulot and I need a vacation)) `
`((?P MR HULOT AND I) (?X A VACATION))`
也就是说，当模式和输入都是列表而且模式的第一个元素是一个段变量，之后变量将会匹配输入的一些初始部分，模式余下的部分将会尝试匹配其余的部分。我们可以更新pat-match，通过增加一个单个cond语句来实现。定义一个断言来测试段变量也是很简单的：
(defun pat-match (pattern input &optional (bindings no-bindings))
 “Mtach pattern against input in the context of the bindings”
  (cond ((eq bindings fail) fial)
    ((variable-p pattern)
    (match-variable pattern input bindings))
    ((eql pattern input) bindings)
    ((segment-pattern-p pattern)
    (segment-match pattern input bindings))
    ((and (consp pattern) (consp input))
    (pat-match (rest pattern) (rest input)
      (pat-match (first pattern) (first input)
       bindings)))
    (t fail)))

`(defun segment-pattern-p (pattern)`
 `“Is this a segment matching pattern: ((?* var) . pat)”`
 `(and (consp pattern)`
   `(starts-with (first pattern) ‘?*)))`

在写segment-match的过程中，重要的问题就是输入中油多少是段变量应该匹配的。其中一个答案是看模式的下一个元素（段变量的下一个元素），并且看在输入中出现的位置。如果没有出现整个模式就会失败，就会返回fail，如果确实出现了，调用它的位置pos。我们将要匹配的变量是针对从输入的开头开始一直到pos。但首先我们要看，模式的rest部分是不是和输入的rest匹配。这个事情交给pat-match的递归调用。递归调用的结果称作b2。如果b2是成功的，之后我们继续并且匹配针对初始的子序列匹配段变量。
技巧是用在b2失败的时候。我们不必完全放弃，因为可能段变量还会匹配输入中更长的一个子序列，之后模式的rest部分就可以匹配输入的rest部分了。我已我们要做的是再一次尝试segment-match，但是为变量强制定一个更长的匹配。可以通过引入一个可选参数start来实现，初始化为0，每次失败都会加一。请注意，这个策略排除了其他变量可以跟在段变量后面的可能性（之后我们会移除这个限制）。

`(defun segment-match (pattern input bindings &optional (start 0))`
 `“Match the segment pattern ((?* var)  pat) against input.”`
 `(let ((var (second (first pattern)))`
    `(pat (rest pattern)))`
   `(if (null pat)`
     `(match-variable var input bindings)`
     `;; We assume that pat starts with a constant`
     `;; In other words, a pattern can’t have 2 consecutive vars`
     `(let ((pos (position (first pat) input`
               `:start start :test #’equal)))`
       `(if (null pos)`
         `fail`
         `(let ((b2 (pat-match pat (subseq input pos) bindings)))`
           `;; If this match failed. Try another longer one`
           `;; If it worked, check that the variables match`
           `(if (eq b2 fail)`
             `(segment-match pattern input bindings (+ pos 1))`
             `(match-variable var (subseq input 0 pos) b2))))))))`

下面是一些段匹配的例子：
`> (pat-match '((?* ?p) need (?* ?x)) `
`'(Mr Hulot and I need a vacation)) `
`((?P MR HULOT AND I) (?X A VACATION)) `
`¬> (pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool)) `
`((?X WHAT HE IS) (?Y FOOL))`
上面例子中的第一个是相当简单的：？p匹配的是need之前的所有东西，？X匹配的就是剩下的部分。后面的例子就包含了更贱复杂一些的后备状态。首先？x会匹配is之前的所有东西（也就是位置2，从0开始数的话）。但是之后模式a不能匹配输入is，所以segment-match就再从位置3开始匹配。这一次就都OK了，is匹配了is，a匹配了a，后面的变量匹配了fool。
很可惜的是，这个版本的segment-match并没有想象中的那么完美，看看下面的例子：
`> (pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b)) => NIL`
这次失败的原因是，？X对应了子序列(1 2)，之后剩余的模式成功对应了剩余的输入，但是最终对match-variable的调用失败了，因为？x有两个值。修正的方法是在测试b2是否失败之前调用match-variable，之后我们就肯定可以再次尝试segment-match，无论是有多长的匹配，也不管引起失败的原因是什么了。

`(defun segment-match (pattern input bindings &optional (start 0))`
 `“Match the segment pattern ((?* var) . pat) against input.”`
 `(let ((var (second (first pattern)))`
     `(pat (rest pattern)))`
   `(if (null pat)`
     `(match-variable var input bindings)`
     `;; We assume that pat starts with a constant`
     `;; In other words, a pattern can’t have 2 consecutive vars`
     `(let ((pos (position (first pat) input`
                 `:start start :test #’equla)))`
       `(if (null pos)`
         `fail`
         `(let ((b2 (pat-match`
               `pat (subseq input pos)`
               `(match-variable var (subseq input 0 pos)`
                   `bindings))))`
           `;; If this match failed, try another longer one`
           `(if (eq b2 fail)`
             `(segment-match pattern input bindings (+ pos 1))`
             `B2)))))))`

现在我们再来看看匹配过程：
`> (pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b)) `
`((?X 1 2 A B))`
请注意，这个segment-match的版本是首先尝试匹配最短可能，还可以设置成尝试匹配最长可能的模式。
