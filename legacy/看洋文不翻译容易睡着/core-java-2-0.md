第二章说java的编程环境
安装JDK
选一个开发环境
使用命令行工具
使用集成开发环境
运行图形程序
构建和运行applets
######安装JDk
很多很多的版本在oracle的官网上都有的下载，总有一款适合你。http://www.oracle.com/technetwork/java/javase/downloads/index.html
这里面的版本很多，除了OS的区别之外，还有JDK本身的版本，SE,EE,JRE,ME,老版本的JDK，从1.2到1.4版本的名字叫做java sdk。
######下载好JDK需要设置环境变量，Path
######之后还要安装库的源文件和文档
JDK文档的下载链接
http://www.oracle.com/technetwork/java/javase/downloads/index.html
######下载书中的样例代码
http://horstmann.com/corejava.html
######java目录导航
看看jdk目录里面的东西都是什么
bin 编译器和工具
demo 样例
docs   html格式的库文档
include  编译本地方法的文件
lib    库文件
src   库源文件

主要需要学习的是两个文件夹，一个是docs，这历史所有的api文档，src是所有的源文件，可以看到类的内部工作。
######选择一个开发环境
如果你打VS开发环境而来，想必已经习惯了在IDE里标记代码，点击鼠标运行的生活，但是在这里我们什么都没有，血药在命令行里输入所有的命令，这听上去是很累赘，但是却是一个基本的技能。装好JDK， 应该就是开始寻找一个开发环境了，但是这些还是等到熟悉编译和运行之后再说。
 主要的开发环境是Eclipse和NetBeans。
######使用命令行工具
编译运行
javac 编译源文件
java 执行程序
######解决问题的小贴士
如果你是手动键入程序，请注意大小写，特别是类名不要写错了
编译程序是需要后缀名.java的，但是运行程序是不需要后缀的。
如果报的错误是没有找到文件，或者不是内外部命令，请检查安装环境是否正确，环境变量的配置
请注意文件的后缀名，因为windows可能会隐藏已知文件类型的后缀
更详细的问题出现qa在这里会有
http://docs.oracle.com/javase/tutorial/getStarted/cupojava/
######用用看集成开发环境
我也是不明白，为什么其他程序不像eclipse一样，解压使用，删除就不留痕迹，多好啊。
######运行一个图形程序
######构建和运行一个Applets
老是运行不了，浏览器因为安全设置组织了程序运行，想想先放一放吧，这个技术暂时没什么现实意义了。







