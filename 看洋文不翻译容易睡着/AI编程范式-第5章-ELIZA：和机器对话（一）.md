#####第5章 ELIZA：和机器对话
解释就是掩饰，越抹越黑。
——Joseph Weizenbaum
MIT 计算机科学家
本章和第一部分的地域章节会着重测试在60年代更加知名的三个AI程序。ELIZA会模拟一个心理治疗师来和用户进行对话。STUDENT解决的是在高中代数书上找到的某种问题，MACSYMA解决的是各种符号数学问题，包括分数和整数计算。前两个程序我们根据之前版本的大部分特性来开发自己的版本，第三个程序我们会实现一个程序功能的精简版本。
所有的这三个程序在一种叫做模式匹配的技术中非常有用。第一部分就是致力于展现这种技术的丰富，当然也有一定的局限。
在这三个程序中，头两个的输入是普通的英语，后两个解决的问题是不凡的数学问题，所以基本上可以称他们是智能的。另一方面，我们应该认识到这种智能很大程度上是一种幻像，ELIZA实际上就是被设计来展示这种幻像的，并不是一个严谨的AI程序。
ELIZA是第一个把英语作为输入和输出的程序，这是他的一大特性。这个程序的名字来源于《卖花女》中的女主角，她由以为杰出的女教师来教授英语。ELIZA最初的开发者，MIT的Joseph Weizenbaum教授，在1966年一月写出了一篇关于ELIZA的论文，其中有一些详细的介绍：
>据说解释就是掩饰，越抹越黑。这句格言用在计算机编程上真的是再合适不过，特别是对启发式编程和人工智能来说。即使对那些按照特定方式制造的机器，也足以让有经验的观察者炫目着迷。但是一旦显露出特定的程序，一旦内部的工作被解释成足以理解的普通语言的时候，就会神奇地崩溃。他代表的仅仅是透露每一个可以理解的过程的集合。观察着会对自己说，我本来可以写成的。带着这种想法，他把程序从智能的架子上拿下来，当做古董，仅仅展示给那些不比他聪明多少的人。
本论文的目标就是为了引起对于关于程序被解释的再评价。很少有程序需要更多了。

我来指出本书的主要目的就是让读者对自己说，我本可以写出他们的。这样的想法就是因为读者已经掌握了某些东西仅仅是因为针对问题的程序失去了炫目的能力罢了。
我们从Weizenbaum教授论文中的一个ELIZA对话实例开始说起。ELIZA的设计师由多种脚本构成的，或者说是多种交互的模式。事实上，还包括一些德语的和威尔士语的脚本。我们使用的ELIZA脚本是基于英语的，他模拟的是一个个人心理治疗师的角色。个人心理治疗师非导向性的，他让患者更加暴露自己。这是一种ELIZA很容易实现的模式，以为他只要保持积极和回答反映就好了，而不需要上给出新的信息。在下面的列表中，用户的输入是大小写混搭的，ELIZA的回答都是大写的：
Men are all alike. （患者：男人都一样）
IN WHAT WAY （ELIZA：哪方面来说）
They're always bugging us about something or another. （患者：他们总是说我们这儿不好的那儿不好）
CAN YOU BE MORE SPECIFIC （ELIZA：能更加具体一些么？）
Well, my boyfriend made me come here. （患者：恩，我男朋友让我到这里来的）
YOUR BOYFRIEND MADE YOU COME HERE （ELIZA：你男票让你来的？）
He says I'm depressed much of the time. （患者：他说我大多时候都很沮丧）
I AM SORRY TO HEAR YOU ARE DEPRESSED（ELIZA：沮丧么？我很抱歉）
It's true. I am unhappy. （患者：这是真的，我很不高兴）
DO YOU THINK COMING HERE WILL HELP YOU NOT TO BE UNHAPPY （ELIZA：你觉得来这里可以让你高兴起来）
I need some help, that much seems certain. （患者：我觉得我需要帮助，这是肯定的）
WHAT WOULD IT MEAN TO YOU IF YOU GOT SOME HELP （ELIZA：对你来说什么会对你有帮助呢）
Perhaps I could learn to get along with my mother. （患者：也许我该试着和我妈妈好好相处）
TELL ME MORE ABOUT YOUR FAMILY（ELIZA：跟我说说你的妈妈）

这个对话令人印象深刻，程序对很多的回答都比较合理，甚至可以说是“善解人意”的。但是正如Weizenbaum解释说的，这种表现是一种幻像，程序对应一些困难的理解的时候，会通过仔细的分析，转化，对输入一步步回应。整个过程就是在寻找特定的模式，基于输入的关键词或者单词。例如，程序检索到输入中有单词，alike或者same，就会打印相似的信息来询问，in what way。其他的模式或许更加复杂。在后面的谈话中，ELIZA首先会放弃逗号后面的所有信息，使用模式搜索关于I need X的信息反馈，转化成输出what would it mean to you if you got X，X在这里就是some help。这个方法最大的好处就是每一个问题都会带来更多的输入：需要一个假期就会返回有了假期的话对你意味着什么？对于程序来说这都是自动完成的，假期和帮助没有什么区别。这个程序的缺点在于有时候会做出一些愚蠢的回答。我需要这个就像我的脑袋上有个洞一样，程序就会回答，你的脑袋上有个洞的话对你意味着什么？，事实上，程序仅仅会对那些有意义的输入产生有意义的输出，还要有人很好的解释答案才行。对于没有意义的输入，程序会照本宣科，不顾是错误的回答。另一个原始ELIZA程序的例子就是和另一个程序PARRY进行通信，这个程序是由Kenneth Colby博士根据模仿偏执行为写成的。一部分对话摘录如下：
I don't understand your motives. （PARRY：我不明白你的动机）
WHY DON'T YOU UNDERSTAND MY MOTIVES? （ELIZA：为什么你不明白我的动机）
Shouldn't I be? （PARRY：我应该明白么？）
WHY DO YOU SAY THAT JUST NOW? （ELIZA：你刚说的是什么意思？）
That's just the way it is. （PARRY：字面意思）
SUPPOSE THAT'S JUST THE WAY IT WERE NOT. （ELIZA：假设是不是字面的意思）
So? （PARRY：所以呢？）
I AM NOT SURE I UNDERSTAND YOU FULLY. （ELIZA：我不确定我明白你的意思）
You harp on the same questions too much. （PARRY：你太执着了，死抓一个问题）
WHAT DOES THAT SIMILARITY SUGGEST TO YOU? （ELIZA：有什么相似的可说的）
I don't get you. （PARRY：我没明白你的意思）
DO YOU WISH THAT YOU DID GET I?（ELIZA：你是希望我明白我自己？）

这表明ELIZA和PARRY对对话都没有深入的理解。当有人类来承担大部分对话中信息的理解的时候，程序还算是可以，但是人类的角色从对话中拿掉，程序的弱点就开始暴露了。
