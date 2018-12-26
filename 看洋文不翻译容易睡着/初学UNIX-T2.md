#####2.1 拷贝文件
cp（copy）
cp 文件1 文件2，是将当前文件1做一份拷贝，文件2.
我们要做的事情是将一个系统中的文件，用cp命令拷贝到unixstuff目录中。
首先，切到用户目录中。
`% cd ~/unixstuff`
之后是在提示符下输入
`% cp /vol/examples/tutorial/science.txt .`
注意：不要忘记最后的点。那是当前目录的意思
上面的命令意思是将science文件拷贝到当前目录，文件名不变
注意：目录 /vol/examples/tutorial/是一个任何人都可以访问拷贝的目录。

习题 2a
创建一个science的拷贝，名字叫做science.bak

#####2.2 移动文件
mv（move）
mv 文件1 文件2 
将文件1移动（或者说重命名）到文件2
使用mv命令，可以将文件从一个地方移动到另一个地方。由于是移动而不是拷贝，所以最终的文件只会剩下一份。
这个命令也可以用在重命名上，将文件移动到同一个目录下，改一个名字。
我们现在把science文件移动到backup目录。
首先切换到unixstuff目录下，之后输入：
`% mv science.bak backups/.`

#####2.3 删除文件和目录
rm（remove），rmdir（remove directory）
删除一个文件，可以使用rm命令。先创建一个science.txt的拷贝，之后删除他。
`% cp science.txt tempfile.txt`
`% ls `，检查是不是已经创建了
`% rm tempfile.txt`
`% ls `，检查是不是已经删除了
rmdir可以删除目录（前提是目录是空的），尝试删除backups目录，删除一个非空的文件夹会报错。

习题 2b
用mkdir创建一个目录tempstuff，然后用rmdir来删除。

#####2.4 在屏幕上显示文件内容
clear （清屏）
在开始下一节之前，我们先用clear命令来清楚之前命令遗留下来的输出信息。在提示符下
`% clear`
这个命令会清楚所有的文本信息，只在窗口屏幕上留下提示符。

cat（concatenate）
cat命令可以用来显示文件的内容
`% cat science.txt`
很明显，文件的长度远远超过了屏幕的长度，可以向上滚动来查看文件。

less
这个命令会展示一个屏幕长度的文件内容，
`% less science.txt`
如果接着阅读可以使用空格键[Space]向下翻页，[q]键可以退出阅读，在文件很长的时候less比cat要好用。

head
会在屏幕上打印文件头十行的内容。
`% head science.txt`
也可以指定要打印的行数
`% head -5 science.txt`

tail
会在屏幕上打印文件内容的最后十行。
`% tail science.txt`

#####2.5 检索文件内容
使用less进行简单检索
用关键字（模式）可以再less中进行简单检索，比如在science.txt文件中检索关键字science。
`% less science.txt`
之后依然处在less程序中（只要不按[q]键退出），按下斜线[/]键，之后输入要检索的关键字。
`/science`
在关键字坐在的地方就会出现高亮提示，按下[n]键来跳转到下一个关键字出现的位置。

grep （别问为什么他叫grep）
grep是UNIX标准组件之一。使用特定的单子或者模式来检索文件内容。
`% grep science science.txt`
grep会打印出每一个包含关键字的行
但是如果文件中没有关键字存在的话
`% grep Sceince science.txt`
grep命令是大小写敏感的，所以Science和science是两个不同的关键字。
如果要忽略大小写区别的话，使用-i选项就可以了。
`% grep -i science science.txt`
检索多个单词的时候需要用单引号来封装。
`% grep -i ‘spinning top‘ science.txt`
grep的一些其它选项是
打印那些不匹配的行，-v
前面加上行号-n
只打印匹配的行的数目-c
你可以在一个命令中使用多个选项。

wc（word count）
wc是一个非常方便的小工具，可以用开进行单词计数
`% wc -w science.txt`
也可以用来计算行数
`% wc -l science.txt`

#####小结
命令|功能
---|---
cp 文件名1 文件名2|拷贝文件1成文件2
mv 文件名1 文件名2|将文件1移动或者重命名成文件2
rm 文件名|删除文件
rmdir 目录名|删除目录
cat 文件名|显示一个文件
less 文件名|一次展示一页文件内容
head 文件名|展示一个文件的头十行
tail 文件名|展示文件的最后几行
grep ’关键字’ 文件名|在文件中检索关键字
wc 文件名|计算文件中的单词数/字符数/行数
