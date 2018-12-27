#####第九章 内存管理
本章将介绍如何使用OC和Cocoa进行内存管理。内存管理是程序设计中常见的资源管理（resource management）的一部分。每个计

算机系统可供程序使用的资源是有限的，包括内存，打开的文件以及网络连接等。如果使用了某种资源，比如因打开文件而占用了

资源，那么需要随后对其进行清理（这种情况下，关闭文件即可）。如果你不断打开文件并且保持打开状态而从不关闭，最终将耗

尽文件资源（在这种情况下，关闭文件即可）。如果你不断打开文件并且保持打开状态而从不关闭，最终将耗尽文件资源。以公共

图书馆为例，假如每个人都只借不还，那么图书馆最终将会因为无书可借而倒闭，每个人也都无法再使用图书馆，任何人都不希望

出现这种结果。
虽说当程序运行结束时，操作系统将收回其占用的资源，但只要程序还在运行，它就会一直占用资源。如果不进行清理，某些资源

最终将被耗尽，程序有可能会崩溃。而且随着操作系统的发展，程序何时终止运行会变得更加难以捉摸。
虽然不是每个程序都会使用文件会或网络连接，但是都会消耗内存。每个C语言程序员都会遇到与内存相关的错误，这种错误是灾难

性的。而是云龙Java和脚本语言的程序员则不需要考虑此类问题；这些语言的内存管理Hi自动进行的，就像父母会给孩子打扫房间

一样。另一方面，我们必须确保在需要的时候分配内存，在程序运行结束时释放占用的内存。如果我们只分配而不释放内存，则会

发生内存泄漏（leak memory）；程序的内存占用量不断增加，最终会被耗尽导致程序崩溃。童谣需要注意的是，不要使用任何刚刚

释放的内存，否则可能误用陈旧的数据，从而引发各种各样的错误，而且如果内存已经加载了其他数据，将会破坏这些新数据。
说明：内存管理不是一个容易解决的问题。虽然Cocoa的解决方案非常简洁，但是想精通它还需要费些时日。即使是有几十年经验的

程序员，第一次遇到这种情况时也会有一定的困难，因此，如果你一时未能完全读懂，请不必担心。
#####9.1 对象生命周期
正如现实世界中的鸟类和蜜蜂一样，程序中的对象也有生命周期。对象的生命周期包括诞生（通过alloc或者new方法实现），生存

（接收消息并执行操作），交友（通过复合以及向方法传递参数）以及最终死去（被释放掉）。当生命周期结束时，他们的原材料

（内存）将被回收以供新的对象使用。
9.1.1 引用计数
现在，对象合适诞生我们已经很清楚了，而且也讨论了如何使用对象，但是怎么知道对象生命周期结束了呢？Cocoa采用了一种叫做

引用计数（reference counting）的计数，又是也叫做保留计数（retain counting）。每个对象都有一个与之相关联的整数墓碑承

租哟它的引用计数器或者保留计数器。当某段代码需要访问一个对象时，将对象的保留计数器减1，表示它不再访问该对象。当保留

计数器的值为0的时候，表示不再有代码访问该对象（可怜的对象），因此他将会被销毁，其占用的内存会被系统回收以便重用。
当使用alloc，new方法或者通过copy消息（接收到消息的对象会创建一个自身的副本）穿件一个对象时，对象的保留计数器的值被

设置为1.要增加对象的保留计数器的值，可以给对象发送一条retain消息。要减少的话，可以给对象发送一条release消息。
当一个对象因其保留计数器归0而即将被销毁时，OC会自动向对象发送一条dealloc消息。你可以在自己的对象中重写dealloc方法，

这样就能释放掉已经分配的全部相关资源，一定不要直接调用dealloc方法，OC会在需要销毁对象时自动调用它。
要活的保留计数器当前的值，可以发送retainCount消息。下面是retain，release和retainCount的方法声明。
- （id）retain；
- （oneway void） release；
- （NSUInteger） retainCount；
retain方法返回一个id类型的值。通过这种方式，可以再接受其他消息的同时进行retain调用，增加对象的保留计数器的值并要求

对象完成某种操作。例如，[[car retain] setTire: tire atIndex: 2];表示要求car对象将其保留计数器的值加1并且执行setTire

操作。
本章的第一个项目是RetainCount1，一下正旭创建了一个RetainTracker类的对象，该对象在初始化和销毁时调用了NSLog（）函数

。
@interface RetainTracker : NSObject
@end // RetainTracker

@implementation RetainTracker
- (id) init
{
if (self = [super init]) { NSLog (@"init: Retain count of %d.",
[self retainCount]);
}
return (self);
}// init

- (void) dealloc
{
NSLog(@"dealloc called.Bye Bye.");
[super dealloc];
} // dealloc
@end // RetainTracker
init方法遵循标准的Cocoa对象初始化方式，我们将在下一章中讨论这种方式。正如前面所讲，当对象的保留计数器的值归0时，将

自动发送dealloc消息（dealloc方法也会被调用）。在我们的例子中，init和dealloc这两个方法是用NSLog（）输出一条消息，表

明他们被调用了。
在main（）函数中我们创建了一个新的RetainTracker类的对象，并间接调用了由RetainTracker类定义的两个方法，当一个新的

RetainTracker类的对象创建完毕后，就会向它发送retain消息或者release消息，以增加或减少对象的保留计数器的值，以下是

NSLog（）函数有趣的输出结果。
int main(int argc, const char * argv[]){
RetainTracker *tracker = [RetainTracker new];
// counr: 1

[tracker retain];//count : 2
NSLog(@"%d", [tracker retainCount]);

[tracker retain];//count : 3
NSLog(@"%d", [tracker retainCount]);

[tracker release];//count : 2
NSLog(@"%d", [tracker retainCount]);

[tracker release];//count : 1
NSLog(@"%d", [tracker retainCount]);

[tracker retain];//count : 2
NSLog(@"%d", [tracker retainCount]);

[tracker release];//count : 1
NSLog(@"%d", [tracker retainCount]);

[tracker release];//count : 0
NSLog(@"%d", [tracker retainCount]);

[tracker release]; // count : 0, dealloc it
return (0);
} // main

当然，在实际开发的过程中，你不会像这样在一个函数中多次保留和释放对象。在其生命周期中，随着程序的运行，对象可能回响在本示例中一样，经历由程序的不同调用而引起的一系列保留和释放行为。
因此，如果对一个对象使用了alloc，new或者copy操作，释放该对象就能销毁它并回收它索占用的内存。
