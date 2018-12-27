第八章Foundation Kit介绍

OC是一门非常精巧实用的语言，目前我们还没有研究完它提供的全部功能。不过现在，我们先探索另一个方向，快速了解一下Cocoa中的Foundation框架。尽管Foundation框架只是Cocoa的一部分，没有内置于OC语言中，但是它依然十分重要，在本书中有必要对它进行讲解。

通过第2章我们知道，Cocoa实际上是由许多个不同的框架组成的，其中最常用于桌面端（OS X）应用程序的是Foundation和Application Kit。它包含了所有的用户界面（UI）对象和高级类，在第16章你会接触AppKit。

如果打算开发iOS平台上的应用程序，那么你将会用到User Interface Kit（UI Kit）框架。我们将在第15章讲解UIKit。UIKit可与iOS平台的AppKit框架相提并论，它包含iOS应用程序索需要的所有界面对象。

#####8.1 稳固的Foundation

Foundation，顾名思义，就是两类UI框架的基础，因为它不包含UI对象，所以它的对象可以在iOS和OS X应用程序中兼容。

Foundation框架中有很多有用的，面向数据的简单类和数据类型，我们将会讨论其中的一部分，例如NSString，NSArray，NSEnumberator和NSNumber。Foundation框架拥有100多个类，所有的类都可以在Xcode安装文档中找到。你可以在Xcode的Organizer窗口选择Documentation选项卡，来查看这些文档。

Foundation框架式以另一个框架CoreFoundation为基础创建的。CoreFoundation框架是用纯C语言写的，你愿意的话也可以使用它，不过本书不予讨论。在介绍名称相似的框架时不要混淆。如果函数或变量的名称以CF开头，那么他们就是CoreFoundation框架中的。其中很多都可以在Foundation框架中找到相对应的，他们之间的转换也非常简单。

#####8.2 使用项目样本代码

再继续学习之前，有一点需要注意，即在本章以及以后章节将提到的项目中，我们仍会创建基于Foundation模板的项目，不过，一下默认的样本代码都会保留。

#import 

int main(int argc, const char * argv[])

{
@autoreleasepool
{//insert code here...
NSLog(@"Hello,World!");
}
return 0;
}

来看一下这段代码。main函数后面首先是关键字@autoreleasepool，并且所有代码都写在了关键字和return语句之间的两个大括号中。这只是Cocoa内存管理的冰山一角，下一章会详细介绍。所以现在，你只要一笑而过就好了，于@autoreleasepool相关的内容先暂时忽略掉。当然不是用这个关键字到不至于引发问题，只不过在程序运行时会冒出一些奇怪的信息。

 

#####8.3 一些有用的数据类型

在深入研究之前，先看看Cocoa为我们提供的一些结构体（struct）。

######8.3.1 范围

第一个结构体是NSRange。

typedef struct _NSRange

{
         unsignedint location;
         unsignedint length;
} NSRange;

这个结构体用来表示相关事物的范围，通常是字符串里的字符范围或者是数组里的元素范围。Location字段存放该范围的起始位置，而length字段则是该范围内所含元素的个数。在字符串“Objective-C is a cool language”中，单词cool可以用location为17，length为4的范围来表示。location还可以用NSNotFound这个值来表示没有范围，比如变量没有初始化。

创建新的NSRange有三种方式，第一种，直接给字段赋值：

NSRange range;

range.location = 17;

range.length = 4;

第二种，应用C语言的聚合结构赋值机制（是不是听起来很耳熟）：

NSRange range = { 17, 4 };

第三种方式是Cocoa提供的一个快捷函数NSMakeRange()：

NSRange range = NSMakeRange ( 17, 4 );

适应NSMakeRange()的好处是你可以在任何能够使用函数的的地方使用它，例如在方法调用中将其作为参数进行传递。

[anObject flarbulateWithRange: NSMakeRange
(13, 15)];

#####8.3.2 几何数据类型

之后你会经常看到用来处理几何图形的数据类型，他们的明曾都带有CG前缀，如CGPoint和CGSize。这些类型是由Core Graphics框架提供的，用来进行2D渲染。Core Graphics使用C语言写的，因此可以在代码中使用C语言的数据类型。CGPoint表示的是笛卡尔平面中的一个坐标(x, y)。

struct CGPoint

{
float x;
float y;
};
CGSize用来存储长度和宽度：
struct CGSize
{
float width;
float height;
};

在之前的Shapes相关的程序中，我们本可以使用一个CGPoint和一个CGSize而不是用自定义的表示矩形的struct来表示形状，不过当时我们想让程序尽可能的简单。Cocoa提供了一个矩形数据类型，它由坐标和大小复合而成。

struct CGRect
{
CGPoint origin;
CGSize size;
};

Cocoa 也为我们提供了创建这些数据类型的快捷函数：CGPointMake(),CGSizeMake(),CGRectMake()。

说明：为什么这些数据类型是C的struct结构体而不是对象呢？原因就在于性能。程序（尤其是GUI程序）会用到许多临时的坐标，大小和矩形区域来完成工作。记住，所有的OC对象都是动态分配的，而动态分配时一个代价较大的操作，她会消耗大量的时间。所以将这些机构提创建成第一级的对象会在使用过程中大增加系统开销。
