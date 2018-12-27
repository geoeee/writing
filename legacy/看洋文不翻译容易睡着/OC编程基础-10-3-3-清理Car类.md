#####10.3.3 清理Car类
我们使用NSMutableArray来替代Car类中唱过的C数组，因为这样就不用执行上限检查了。此外，我们修改了Car类的@interface部分，以便他可以支持可变数组。
@interface Car : NSObject
{
NSMutableArray: *tires;
Engine*engine;
}

我们几乎修改了Car类中的每一个方法，一时期遵循内训管理规则，先来看一下ninit方法。
-（id）init
{
if (self = [super init]){
tires = [[NSMutableArray alloc] init];
for (int i = 0; i < 4; i++){
[tires addObject: [NSNull null]];
}
}
return (self);
}// init
你已经不止一次见到过self = [super init]这种风格应该熟记于心了。就目前你所知道的，它可以确保超累能初始化对象并使其能够能唱运行。
接下来我们创建NSMutableArray数组。在NSMutableArray类里面有个渐变的方法，replaceObjectAtIndex：WithObject：该方法最是用来实现setTire：Atindex；方法。如果要用replaceObjectAtIndex：方法，在指定的索引位置处必须存在一个能够被替换的duixiang.quanxindeNSMutableArray数字不包含热很饿荣，因此我们需要使用一些s对象来作为占位符，二NSNull类的对象非常适合完成此项工作。因此，你不需要使用NSNull类的对象预支NSMutableArray数组，在本例中我们这样做只是为了使随后的实现更加容易。
在init方法结束时，我们返回self，这正是我们刚刚完成初始化的对象。
下面是两个访问方法setEngine：和engine。setEngine：使用的是之前讲过的“保留以前已传入的对象并且释放当前对象的技术”。
- (void) setEngine: (Engine *)new Engine
{
[newEngine retain];
[engine release];
engine = newEngine;
} //setEngine 
而engine的访问方法只是简单滴返回了dangqiandeengine对象
-(Engine *) engine
{
return (engine);
} //engine
现在我们来实现tire对象的访问方法，首先是setter方法
 -(void)setTire: (Tire *) tire
atIndex:(int) index
{
[tires replaceObjectAtIndex:index withObject:tire];
} //setTire:atIndex:
setTire:方法使用replaceObjectAtIndex：withObject：从数组结合中删除现有对象并用新对象替代。因为NSMutableArray数组会自动保留新的tire对象并释放索引位置上德对象（无论该对象时NSNull占位符还是tire对象），所以对于tire对象不需要执行其他内存管理。当NSMutableArray数组被销毁时，他讲释放数组中的所有对象，因此tire对象会被清理。
getter方法tireAtIndex：使用了由NSArray类提供的objectAtIndex：
方法从数组中获取tire对象；
- (Tire *) tireAtIndex: (int) index
{
Tire *tire;
tire = [tires objectAtIndex: index];
return (tire);}
######快速返回
可以直接返回objectAtIndex：方法的结果值，将如下方法简化为一行，这完全符合语法。
(Tire *) tireAtIndex: (int) index
{
return ([tires objectAtIndex: index]);
} //tireAtIndex;
不过在原来的tireAtIndex方法中引入额外的变量，可以使我们的代码更加容易阅读，（至少我们来说是这样的），使得设置断点更加方便，因此我们可以明白该方法的返回的是哪个对象。使用这种技术也是的原始调试在调用objectAtIndex方法到该方法返回tire对象的过程中输出NSLog（）更加容易、
我们仍然需要确保car能够清理保留的对象，尤其是engine和tire数组，而dealloc方法就是可以完成此类工作，、
- (void) dealloc
{
[tires release];
[engine release];
[super dealloc];
} // dealloc
这可以确保在该car独享被销毁时素偶ideaM内存都将被税收。一定要调用超类dedealloc方法，这一点很容易被忽视。并且要确保[super dealloc]是dealloc方法的最后一条语句。
最后是car对象的print方法，该方法输出体热对象和engine对象。
- （void）print
{
for (inti = 0; i < 4; i++)
{
NSLog(@"%@",[self tireAtIndex:i]);
}
NSLog(@"%@", engine);
} //print
循环中的print方法一次输出每个体热对象的描述并将他们显示出来。有趣的是，该循环使用的是tireAtIndex：方法简介取值，而不是直接从数组中取值。如果希望直接访问NSMutableArray数组，完全可以这么做。不过如果你是用的访问方法，（即便是在类的视线中），应该让代码不收将来更改的影响，。比方说，tire对象的存储机制以后放生了变化（例如又回到了之前的C语言等额数组），我们就不需要再去修改print方法了。

######10.4 Car类的内存清理（垃圾回收方式和ARC方式）
那么垃圾回收是如何执行的呢？在支持垃圾回收的OC中，Car类是什么样子的呢? setEngine方法变得更简单。
- (void) setEngine: (Engine *) newEngine
{
engine = newEngine;
}  // setEngine
我们修改了engine对象的实例变量。当Cocoa的垃圾回收机制运行时，它知道没有其他实例变量只想原来的engine对象，因此，垃圾回收器销毁了原来的engine对象。因此，垃圾回收期销毁了原来的engine对象，另一方当面，由于有一个实例变量指向了newEngine对象，所以newengine对爱那个不会被回收。来及回收期知道右边梁正在使用newEngine对象，
dealloc方法完全消失了，因为在支持垃圾回收的OC中，dealloc方法毫无用武之地。如果需要在小慧对象时执行某些操作，则需要冲下-finalize方法，当对象最终被回收是该方法会被调用。虽说使用-finalize方法会引起一些细小的问题，不过对于Cocoa环境下的程序设计来说你无需担心。
ARC版本与垃圾回收版本类似。我们唯一要加的就是@autopreleasepool，来告诉编译器我们想要拆塔来处理保留和释放事件。
######构造便利初始化函数
没有什么代码是完美的。回忆一下为偶们在main（）函数中创建Tire对象方式。
tire = [[Tire alloc] init];
[tire setPressure: 23 + i];
[tire setTreadDepth: 33 - i];
这里使用了4条消息和三行代码。如果能在但此操作中完成此功能，那再好不过了。下面，我们构造了一个能够同时获得轮胎压力和花纹深度的便利初始化函数。带有新增的初始化函数
 - (id) initWithPressure: (float) pressure treadDepth: (float) treadDepth;
毫无疑问，该方法的实现非常简单：
- （id） initWithPressure:(float) p treadDepth: (float) td
{
if (self = [super init]){
pressure = p;
treadDepth = td;
}
return (self);
}
现在，我们可以单步完成tire对象的分配和初始化了
Tire* tire;
tire = [[Tire alloc] 
initWithPressure: 23+ i
treadDepth: 33 - i];
#####10.5 制定初始化函数
不幸的是，不是所有的初始化方法都能够正常运行，挡在类中增加便利初始化函数时，会出现一些小问题。下面，我们在Tire类中增加两个便利初始花函数
- (id) initWithPressure: (float) pressure;
- (id) initWithTreadDepth: (float) treadDepth;
这两个新的初始化函数适用于那些轮胎需要特定压力或者轮花纹深度二不关心其他属性的人第一次便携的初始换方法如下。
- (id) intiWithPressure:(float) p
{
if (self= [super init]){
pressure = p;
treadDepth = 20.0;
}
return (self);
} // initWithPressure

- (id) initWithTreadDepth: (float) td
{
if (self = [super init]){
pressure = 34.0;
treaddepth = td;
}
return(self);
}  //initWidthTreadDepth
现在我们已经有了4个初始化方法；init，initWithPressure，initWithTreadDepth和initWithPressure：treadDepth：每个都知道默认的轮胎压力至和花纹深度值。这些方法能正常工作，代码也都是正确的。
但是在开始子类化Tire类时候，问题就来了。
#####10.5.1 子类化问题
我们已经创建了Tire类的子类AllWeatherRadial，现在假设AllWeatherRadial类希望增加两个实例变量，rainHandling和snowHandling。这两个变量都是浮点数。用于表是轮胎在潮湿和积雪的道路上德性能值。创建一个新的AllWeatherRadial类对象时，必须确保两个变量具有合理的只。因此，带有新的实例变量和访问方法的AllWeatherRadial类股和新的interface的代码如下
@interface AllWeatherRadial: Tire
{
float rainHandling;
float snowHandling;
}
// setter and getter method...
之后重新设置description函数，main（）函数，但是运行程序是，问题出现了。
AllWeatherRadial对象的新变量没有被设置为合理的默认值。哪里出了问题？因为实在inti方法中复制的，所以我们必须重写init方法。但是Tire类中还有initWithPressure等等初始化方法，难道我们必须重现所有这些方法？而且就算这么干了，。之后Tire类中由增加一个新变量怎么办？如果Tire类的改动会影响到AllWeatherRadial类，那么这种设计方式是有问题的。
型号Cocoa的设计人员已经预料到这个问题币不能够提出了从指定初始化函数的概念（designed initializer），即是说，某个初始化方法被指派为制定初始化函数。该类的所有初始化方法都使用制定初始化函数执行初始化操作，而子类试用期超累的制定初始化函数惊醒超累的初始化。通常，接收参数最多的初始化方法是最终的制定初始化函数。如果你使用了其他人缩写的代码，则一定要检查文档，，弄清楚哪个方法是制定初始化函数。
#####10.5.2 Tire类的初始化函数改进后的版本
首先，我们需要确定应该将哪个Tire类的初始化函数指派为制定初始化函数。intiWithPressure：treadDepth：就是一个不错的选择，参数最多，而且在初始化函数中是最灵活的。
为了保证制定初始化函数的顺利执行，所有其他的初始化函数应该按照initWithPressure：treadDepth的形式实现，可能如下
 -（id）init
{
if (self = [super initWithPressure:34 treadDepth: 20])
{}
return (self);
}

......其他初始化函数类似。在其中调用哪个参数最多的函数
说明：你并不需要真的向上面代码一样不谢任何代码。我破门之所以这么做只是为了保持初始化方法具有统一的形式、
#####10.5.3 添加AllWeatherRadial类的初始化函数
现在应该向AllWeatherRadial类中添加制定初始化函数了，我们只需要添加重写的制定初始化函数。
- (id) initWithPressure: (float) p treadDepth: (float) td
{
if (self = [super initWithPressure: p treadDepth: td])
{
rainHandling = 23.7;
snowHandling = 42.5;
}
return (self);
}
这样子程序的初始值设置就有保障了。
