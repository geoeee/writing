#####4.12 新的问题来了：猴子和香蕉问题
为了显示我们的GPS真的是通用的，需要在不同的问题中检验。我们来看一看一个经典的AI问题，最初是由Saul Amarel在1968年提出的。想象下面的场景：一只饥饿的猴子正站在一个房间的门口，在房子的正中间房顶上，用绳子吊着一串香蕉，刚刚好高度是猴子够不着的。在门附近有一把椅子，足够轻便，猴子推得动，也足够高度，猴子可以站在上面拿到香蕉。为了让事情再复杂些，假设猴子手里拿着一个玩具球，而且规定一次只能在手里持一件东西。
在尝试表现这样的场景的时候，我们可以灵活选择把什么放到当前状态，把什么放进操作符。假设我们如此定义操作符：

`(defparameter *banana-ops*`
 `(list`
   `(op ‘climb-on-chair`
     `:preconds ‘(chair-at-middle-room at-middle-room on-floor)`
     `:add-list ‘(at-bananas on-chair)`
     `:del-list ‘(at-middle-room on-floor))`
   `(op ‘push-chair-from-door-to-middle-room`
     `:preconds ‘(chair-at-door at-door)`
     `:add-list ‘(chair-at-middle-room at-middle-room)`
     `:del-list ‘(chair-at-door at-door))`
   `(op ‘walk-from-door-to-middle-room`
     `:preconds ‘(at-door on-floor)`
     `:add-list ‘(at-middle-room)`
     `:del-list ‘(at-door))`
   `(op ‘grasp-bananas`
     `:preconds ‘(at-bananas empty-handed)`
     `:add-list ‘(has-banans)`
     `:del-list ‘(empty-handed))`
   `(op ‘drop-ball`
     `:preconds ‘(has-ball)`
     `:add-list ‘(empty-handed)`
     `:del-list ‘(has-ball))`
   `(op ‘eat-banans`
     `:preconds ‘(has-bananas)`
     `:add-list ‘(empty-handed not-hungry)`
     `:del-list ‘(has-bananas hungry))))`

使用这些运算符，可以展现的问题就是让猴子不再饥饿，给定的初始状态，在门口，站在地板上，拿着球，饥饿，在门口有把椅子。GPS可以找到一个这个问题的解决方案：

`> (use *banana-ops*) => 6 `
`¬> (GPS '(at-door on-floor has-ball hungry chair-at-door) `
`'(not-hungry) ) `
`((START) `
`(EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM) `
`(EXECUTING CLIMB-ON-CHAIR) `
`(EXECUTING DROP-BALL) `
`(EXECUTING GRASP-BANANAS) `
`(EXECUTING EAT-BANANAS))`

这里没有对原来的GPS程序作出任何的改动，仅仅是使用了一个新的操作符集合。
#####4.13 迷宫搜索问题
现在我们来看看另一个经典问题，迷宫搜索。我们假定有一个迷宫，如下图所示：
![Maze.JPG](http://upload-images.jianshu.io/upload_images/46495-2dc9eb454aaa28e4.JPG)
定义一些函数来帮助构建针对问题的操作符，比直接定义一大串操作符要容易得多。下面的代码定义了一般的迷宫操作符集合，以及这个特定迷宫：

`(defun make-maze-ops (pair)`
  `“Make maze ops in both directions”`
  `(list (make-maze-op (first pair) (second pair))`
    `(make-maze-op (second pair) (first pair))))`

`(defun make-maze-op (here there)`
 `“Make an operator to move between two places”`
 `(op ‘(move from ,here to ,there)`
   `:preconds ’((at ,here))`
     `:add-list ‘((at ,there))`
   `:del-list ’((at ,here))))`

`(defparameter *maze-ops*`
 `(mappend #’make-maze-ops`
   `‘((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)`
   `(12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)`
   `(23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))`

现在我们可以使用操作符的列表来解决这个迷宫的一些问题。我们也可以通过给定的连接的列表来创建另一个迷宫。注意没有证据表明迷宫的形式必须是55一层的，他仅仅是一种连接具象化的表示方式。
`> (use *maze-ops*) => 48`
`> (gps '((at 1)) '((at 25))) `
`((START) `
`(EXECUTING (MOVE FROM 1 TO 2)) `
`(EXECUTING (MOVE FROM 2 TO 3)) `
`(EXECUTING (MOVE FROM 3 TO 4)) `
`(EXECUTING (MOVE FROM 4 TO 9)) `
`(EXECUTING (MOVE FROM 9 TO 8)) `
`(EXECUTING (MOVE FROM 8 TO 7)) `
`(EXECUTING (MOVE FROM 7 TO 12)) `
`(EXECUTING (MOVE FROM 12 TO 11)) `
`(EXECUTING (MOVE FROM 11 TO 16)) `
`(EXECUTING (MOVE FROM 16 TO 17)) `
`(EXECUTING (MOVE FROM 17 TO 22)) `
`(EXECUTING (MOVE FROM 22 TO 23)) `
`(EXECUTING (MOVE FROM 23 TO 24)) `
`(EXECUTING (MOVE FROM 24 TO 19)) `
`(EXECUTING (MOVE FROM 19 TO 20)) `
`(EXECUTING (MOVE FROM 20 TO 25)) `
`(AT 25))`

迷宫问题指出了一个隐秘的小bug。我们要GPS返回的是一系列执行的操作的列表。然而，因为考虑到目标达成也可以没有任何操作，所以，在GPS返回值中加上了(START)。这些例子包含了START和EXECUTING形式，但是也包含了一系列的(AT n)。这就是bug。如果我们回过头看看函数GPS，就会发现他的结果是根据achieve-all返回的状态删除所有的原子得来的。这是一个双关，我们所说的移除原子就是移除除了(start),(executing action)之外的形式。到现在为止，所有的这些条件都是原子，所以方法可用。迷宫问题是从形式(AT n)中引入条件，所以首先这就是个问题。基本的精神就是当一个程序员使用双关的时候，也就是将真正发生的事情用比较通俗的话来说，就会出现一些麻烦。我们真正想要做的不是删除原子，而是找到所有知识操作的元素。下面的代码就是主要的意思：
`(defun GPS (state goals &optional (*ops* *ops*))`
 `“General Problem Solver: from state, achieve goals using *ops*.”`
 `(find-all-if #’action-p`
   `(achieve-all (cons ‘(start) state) goals nil)))`

`(defun action-p (x)`
 `“Is x something that is (start) or (executing …)?”`
 `(or (equal x ‘(start)) (executing-p x)))`

迷宫问题也显示出了一个版本2的优点：就是他会返回所做的操作的表现，而不仅仅是打印他们。这样子就可以利用结果来进行一些处理，而不是仅仅看看而已。假设我们想要一个函数，给我们一个穿越迷宫的路径，用一系列的位置表示。我们可以将GPS作为一个子函数调用，之后再操作结果。
`(defun find-path (start end)`
 `“Search a maze for a path from start to end.”`
 `(let ((results (GPS ‘((at ,start)) ’((at ,end)))))`
   `(unless (null results)`
     `(cons start (mapcar #’destination`
       `(remove ‘(start) results`
         `:test #’equal))))))`

`(defun destination (action)`
 `“Find the Y in (executing (move from X to Y))”`
 `(fifth (second action)))`
函数find-path调用GPS来获取results。如果是nil就没有答案，如果不是nil就会提取results的rest部分（也就是移除start）。从每一个形式(executing (move from x to y))中挑选出目的地，y，并且记住起始点。
`> (use *maze-ops*) => 48 `
`¬> (find-path 1 25) => `
`(1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25) `
`> (find-path 1 1) => (1) `
`> (equal (find-path 1 25) (reverse (find-path 25 1))) => T`




