#####4.14 积木世界问题
另一个引起AI领域中广泛关注的问题就是积木世界问题。想像一群在桌面上玩堆积木的孩子。问题就是把当前的积木配置，转化成目标配置。我们假设，一块积木，有且仅有另一块积木在它的上面，即使他们可以聚合成人意的高度。在这个世界中唯一可以进行的操作就是移动一块上面没有其他积木的积木到另一个地方。创建一个可以移动能移动的积木的操作符。

(defun make-block-ops (blocks)
 (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a ‘table b) ops)
          (push (move-op a b ‘table) ops))))
     ops))

(defun move-op (a b c)
  “Make an operator to move A from B to C.”
  (op ‘(move ,a from ,b to ,c)
   :preconds ‘((space on ,a) (space on ,c) (,a on ,b))
   :add-list (move-ons a b c)
   :del-list (move-ons a c b)))

(defun move-ops (a b c)
(if (eq b ‘table)
‘((,a on ,c))
‘((,a on ,c) (space on ,b))))

现在我们尝试用这些操作符解决问题，首先是最简单的情况，把一个积木移动到另一个积木上。

![积木问题中最简单的情况.JPG](http://upload-images.jianshu.io/upload_images/46495-14c547fdab4f13d2.JPG)

`> (use (make-block-ops '(a b))) => 4 `
`> (gps '((a on table) (b on table) (space on a) (space on b) `
`(space on table)) `
`'((a on b) (b on table))) `
`((START) `
`(EXECUTING (MOVE A FROM TABLE TO B)))`

下面的问题更加复杂一些，把两块积木的栈倒过来，这次我们使用调试输出。

![两块积木的顺序倒过来.JPG](http://upload-images.jianshu.io/upload_images/46495-7f0403a5db15f651.JPG)

`> (debug :gps) => (:GPS) `
`> (gps '((a on b) (b on table) (space on a) (space on table)) `
`'((b on a))) `
`Goal: (B ON A) `
`Consider: (MOVE B FROM TABLE TO A) `
`Goal: (SPACE ON B) `
`Consider: (MOVE A FROM B TO TABLE) `
`Goal: (SPACE ON A) `
`Goal: (SPACE ON TABLE) `
`Goal: (A ON B) `
`Action: (MOVE A FROM B TO TABLE) `
`Goal: (SPACE ON A) `
`Goal: (B ON TABLE) `
`Action: (MOVE B FROM TABLE TO A) `
`((START) `
`(EXECUTING (MOVE A FROM B TO TABLE)) `
`(EXECUTING (MOVE B FROM TABLE TO A))) `
`> (undebug) => NIL`

有时候动作连接的顺序也是很重要的。例如，你不可以拥有一块蛋糕的同时也吃掉它，但是你可以先拍照片然后吃掉它，只要在吃之前拍照就可以了，在积木世界我们有这样一个顺序：

![积木移动的顺序.JPG](http://upload-images.jianshu.io/upload_images/46495-27e1c6b5cefa3cd9.JPG)


`> (use (make-block-ops '(a b c))) => 18 `
`> (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) `
`'((b on a) (c on b))) `
`(( START) `
`(EXECUTING (MOVE A FROM B TO TABLE)) `
`(EXECUTING (MOVE B FROM C TO A)) `
`(EXECUTING (MOVE C FROM TABLE TO B)))`
`> (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) `
`'((c on b) (b on a))) `
`¬ NIL`

在第一种情况中，是首先把B放在A上，之后把C放在B上。第二种情况是，程序首先把C放在B上，但是之后要想把B放在A上就会任务失败了。这种被称作先决条件兄弟冲突问题，但是程序不会采取任何行动。我们能做的就是改变连接的顺序，我们可以修改achieve-all如下：
`(defun achieve-all (state goals goal-stack)`
 `“Achieve each goal, trying several orderings.”`
 `(some #’(lambda (goals) (achieve-each state goals goal-stack))`
   `(orderings goals)))`

`(defun achieve-each (state goals goal-stack)`
 `“Achieve each goal, and make sure they still hold at the end.”`
 `(let ((current-state state))`
   `(if (and (every #’(lambda (g)`
         `(setf sucrrent-state`
           `(achieve current-state g goal-stack)))`
       `goals)`
     `(subset goals current-state :test #’equal))`
   `Current-state)))`

`(defun orderings (l)`
 `(if (> (length l) 1)`
   `(list l (reserve l))`
   `(list l)))`

我们可以用几种方式展示，但是会得到同一个答案。注意，我们只考虑两种顺序：给定的顺序和倒转的顺序。显然对于一个或者连个连接的目标集合这就是所有的顺序了。一般来说，如果每一个目标集合都只有一个交互，两个顺序中的其中一个就是可行的。因此，我们假定先决条件兄弟目标冲突问题交互是很罕见的。而且很少有目标集合会超过一种交互。另一种可能性就是考虑所有的目标顺序排列，但是会在规模增长的时候消耗很多时间。
另一个要考虑的就是解决方案的效率。我们看一下，下图中将积木C移动到桌面上的任务：


![移动C到桌面上.JPG](http://upload-images.jianshu.io/upload_images/46495-296006e3a0e0284b.JPG)

`> (gps '((c on a) (a on table) (b on table) `
`(space on c) (space on b) (space on table)) `
`'((c on table))) `
`((START) `
`(EXECUTING (MOVE C FROM A TO B))) `
`(EXECUTING (MOVE C FROM B TO TABLE)))`

谁说方法是正确的，但是还有一个更好的办法就是把C直接移动到桌子上。这个更简单的方法被忽视了是由于一个意外：make-blocks-ops定义的操作符是把C从B上移开要在把C从A上移动到桌面上之前的。所以第一个移动，操作符会尝试，成功的把C放在了B上，因此，两步的解决方法要先于一步的解决方法得到。下面的例子在可以两步搞定的情况下用了四步的解决方法：



![四步解决的方式.JPG](http://upload-images.jianshu.io/upload_images/46495-620dd57cc38e3a7e.JPG)

`> (gps '((c on a) (a on table) (b on table) `
`(space on c) (space on b) (space on table) `
`'((c on table) (a on b))) `
`((START) `
`(EXECUTING (MOVE C FROM A TO B) `
`(EXECUTING (MOVE C FROM B TO TABLE))`
`(EXECUTING (MOVE A FROM TABLE TO C))`
`(EXECUTING (MOVE A FROM C TO B)))`

如何找到更简短的解决方法？一种是我们可以进行全部的搜索：尝试更简短的方法，临时放弃一些看上去更值得推荐的方法，之后在来考虑。这种方式在第六章详细介绍，使用一般搜索函数。一个不太极端的方式是吧操作符检索的结果做一个有限的重新排序：需要更少先决条件的那个就会首先尝试执行。也就是所这意味着满足所有先决条件的操作总是会排在其他操作之前。为了实现这个方法，我们来修改一下achieve：

`(defun achieve (state goal goal-stack)`
 `“A goal is achieved if it already holds,`
 `Or if there is an appropriate op for it that is applicable.”`
 `(dbg-indent :gps (length goal-stack) “Goal :~a” goal)`
 `(cond ((member-equal goal state) state)`
   `((member-equal goal goal-stack) nil)`
 `(t (some #’(lambda (op) (apply-op state goal op goal-stack))`
     `(appropriate-ops goal state)))));***`

`(defun appropriate-ops (goal state)`
 `“Return a list of appropriate opertators,`
 `Sorted by the number of unfulfilled preconditions.”`
 `(sort (copy-list (find-all goal *ops* :test #’appropriate-p)) #’< `
   `:key #’(lambda (op)`
     `(count-if #’(lambda (precond)`
        `(not (member-equal precond state)))`
       `(op-preconds op)))))`

现在我们可以得到想要的解决方案了：
![](http://upload-images.jianshu.io/upload_images/46495-620dd57cc38e3a7e.JPG)

`> (gps '(c on a) (a on table) (b on table) `
`(space on c) (space on b) (space on table)) `
`'(c on table) (a on b))) `
`(( START) `
`(EXECUTING (MOVE C FROM A TO TABLE)) `
`(EXECUTING (MOVE A FROM TABLE TO B)))`

![积木移动的顺序.JPG](http://upload-images.jianshu.io/upload_images/46495-27e1c6b5cefa3cd9.JPG)

`> (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) `
`'((b on a) (c on b))) `
`(START) `
`(EXECUTING (MOVE A FROM B TO TABLE)) `
`(EXECUTING (MOVE B FROM C TO A)) `
`(EXECUTING (MOVE C FROM TABLE TO B))) `
`> (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) `
`'((c on b) (b on a))) `
`(START) `
`(EXECUTING (MOVE A FROM B TO TABLE)) `
`(EXECUTING (MOVE B FROM C TO A)) `
`(EXECUTING (MOVE C FROM TABLE TO B)))`

######萨斯曼异常
其实有的问题是不能通过目标的重新排序来解决的：

![萨斯曼异常.JPG](http://upload-images.jianshu.io/upload_images/46495-7981d7b36550fe58.JPG)

看上去也不是很难么，我们来看看GPS是如何解决的：
`> (setf start '((c on a) (a on table) (b on table) (space on c) `
`(space on b) (space on table))) `
`((C ON A) (A ON TABLE) (B ON TABLE) (SPACE ON C) `
`(SPACE ON B) (SPACE ON TABLE) `
`¬ > (gps start '((a on b) (b on c))) => NIL `
`¬ > (gps start '((b on c) (a on b)) => NIL`

这是一个与叠放顺序没有关系的先决条件兄弟目标冲突问题。换句话说，没有什么计划可以解决两个目标之间的连接。这是一个令人惊异的事实，这个例子也被称为萨斯曼异常，我们会在第六章讨论。
