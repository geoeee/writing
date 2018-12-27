#####其他实用的UNIX命令
######quota
机器上所有的用户都被分配了一定数量的磁盘空间来存储个人的文件，一般大约是100MB左右。如果

检查你的配额，看看还有多少剩余
`% quota -v`

######df
命令df会返回目录所挂载的文件系统的空余信息，例如看看现在的文件服务器上还有多少剩余空间
`% df .`

######du
命令du会输出每一个子目录所占的空间信息，如果你超支了空间的话可以用它来找出是哪一个文件

夹最占空间。
`% du -s *`
标志-s的意思是仅仅以大小来展示，*的意思是所有文件和目录

######gzip
功能是压缩文件大小，释放磁盘空间。
`% gzip science.txt`
压缩的是文件的大小，之后产生压缩文件science.txt.gz
想要解压缩就是用gunzip命令
`% gunzip science.txt.gz`

######zcat
使用这个命令可以在压缩文件不解压缩的情况下查看文件内容
`% zcat science.txt.gz`
如果文件长度输出过长，请使用管道命令输出到less
`% zcat science.txt.gz | less`

######file
分辨出文件包含的数据是什么类型，比如是ascii文本文件，图像文件，还是压缩数据
`% file *`

######diff
比较两个文件的内容，然后展示他们不同的地方。假设你有一个文件file1，编辑其中的一部分内容

之后转存成file2。之后看看两个者的区别
`% diff file1 file2`
由符号<开头的行就是file1的内容，由符号>开头的行是术语file2的内容。

######find
可以根据指定的名字，日期，大小或者任何其他属性来对目录进行检索。这是一个简单的命令，但

是有很多选项可以使用。
在当前目录下检索所有后缀名是.txt的文件，之后将名字打印在屏幕上。
`% find . -name "*.txt" -print`
找出超过1Mb大小的文件
`% find . -size +1M -ls`

######history
C shell会将你输入过的所有命令保存成一个列表，每一个命令会根据输入的次序赋予一个号码。
`% history (show command history list)`
如果你是使用C Shell的，你可以使用感叹号来调用命令
`% !! (recall last command)`
`% !-3 (recall third most recent command)`
`% !5 (recall 5th command in list)`
`% !grep (recall last command starting with grep)`
你也可以自己定义history命令的缓冲行数大小。
`% set history=100`
