#####13.2.2 复制Tire
Tire类比Engine类更加难以复制。Tire类有两个实例变量（pressure和treadDepth）需要被复制到Tire类的新对象中，而且AllWeatherRadial子类又引入了另外两个实例变量（rainhandling和snowHandling），这两个变量也需要被复制到新对象中。
首先来看一下Tire类，其采用了协议的借口代码如下
@interface Tire : NSObject <NSCopting>
@property float pressure;
@property float treadDepth;
// - methods
@end  //Tire
下面是copyWithZone方法的实现
- （id） copyWithZone: (NSZone *) zone{
Tire *tireCopy;
tireCopy = [[[self class] allocWithZone: zone] initWithPressure:
pressure treadDepth:treadDepth];
return(tireCopy);
}// copyWithZone
该实现中也有[[self class] allocWithZone: zone]这种形式，就像在Engine类中一样，由于在创建爱你对象时必须使用init方法，所以我们可以方便地使用Tire类的initWithPressure：treadDepth：方法，将新的tire对象的pressure和treadDepth设置为我们正在复制的tire对象的值。该方法正还是Tire类的指定初始化函数，但是并不是必须要使用指定初始化函数来制定复制操作。只要你愿意，也可以使用简单的init方法和访问器方法来修改对象的属性。
######方便的指针
你可以像下面那样使用C语言风格的指针运算符直接访问实例变量。
tireCopy->pressure = pressure;
tireCopy->treadDepth = treadDepth;
一般来说，当设置属性不大可能涉及额外工作时，我们尽量使用init方法和访问器方法。
------------------------------------------
下面我们来讨论AllWeatherRadial类，它的借口内容保持不变
@interface AllWeatherRadial ： Tire
// - properties
// - methods
@end // AllWeatherRadial
等一下，协议<NSCopying>去哪里了，你并不需要他了，也许你能想通，当AllWeatherRadial类继承于Tire类的时候，它便已经获得了Tire类的所有属性，包括对NSCopying协议的遵守。
不顾，我们需要重写copyWithZone：方法，因为我们必须确保AllWeatherRadial类中的rainHandling和snowHandling这两个实例变量被复制。
 - (id) copyWithZone: (NSZone *) zone
{
AllWeatherRadial * tireCopy;
tireCopy = [super copyWithZone: zone];
tireCopy.rainHandling = rainHandling;
tireCopy.snowHandling = snowHandling;
return (tireCopy);
} // CopyWithZone
因为AllweatherRadial是一个可以复制的类的子类，所以他既不需要实现allocWithZone方法，也不需要使用前面用过的[self class]形式。AllweatherRadial类知足要请求其父类执行copy操作，并期望父类正确地复制以及在分配对象时使用[self class]技术，因为Tire类的copyWithZone方法会使用[self class]来确定哟复制的对所述的类，所以该方法将创建一个AllweatherRadial类的新对象，这正是我们所期望的。该方法还替我们复制了pressure囧恩treadDepth的值。这样是不是更方便了、。
剩下的工作就是设置rainHandling和snowHandling这两个实例变量的值，访问器方法完全可以圣人这个工作。
####13.2.3 复制Car
由于我们已经能够复制Engine和Tire以及他的子类，下面就来复制ca
r类本身。
如你所料，Car类需要采用NSCopying协议
@interface Car: NSObject <NSCopying>
// properties
// methods
@end // Car
若是遵守NSCopying协议，Car类必须实现我们原来的友元方法“copyWithZone”。下面是Car类的copyWithZone方法的实现。
- (id) copyWithZone: (NSZone *) zone
{
Car * carCopy;
carCopy = [[[self class] allocWithZone: zone] init];
carCopy.name = self.name;
Engine * engineCopy;
engineCopy = [[engine copy] autorelease];
carCopy.engine = engineCopy;
for (int i = 0; i < 4; i++)
{
Tire *tireCopy;
tireCopy = [[self tireAtIndex: i] copy];
[tireCopy autorelease];
[carCopy setTire: tireCopy atIndex:i];
}
return (carCopy);
}// copyWithZone
上面的copyWithZone：方法只比我们以前编写的多了一点代码，但所有的这些代码都与你曾经见过的类似。
首先，通过给正在接受copy罅隙的对象所述的类发送allocWithZone消息分配一个新的car对象。
Car *carCopy；
carCopy= [[[self class] allocWothZone: zone] init];
虽然目前CarParts项目中不包含Car类的子类，但是有朝一日，可能会用新的子类，你永远无法知道何时会有人发明什么车。我们只能通过使用self所述的类来分配新对象以防止味蕾可能出现的问题，就像我们之前做的那样。
我们需要复制car对象的名称；
carCopy.name = self.name;
请记住，name属性复制其字符串对象，因此新的car对象将拥有正确的名称。
接下来，复制engine独享，并通知carCopy使用复制的engine对象作为自己的engine属性。
Engine * engineCopy;
enginCopy = [[engine copy] autorelease];
carCopy.engine = engineCopy;
engine对象为什么要自动释放？有必要这样做么？让我们再看看内存管理问题。[engine copy]将返回一个保留计数器的值为1的对象。setEngine方法将保留接收到的engine对象，并将其保留计数器的值增加为2.当carCopy（最终）被销毁时，Car类的dealloc方法将释放engine对象，因此阿德保留计数器额值又减少为1。到这些事情发生时，这段代码已经运行了很长时间了，因此没有人会出来为其发送最后的release消息使其销毁，。如果那样的话，该engine对象将发生内存谢落，通过自动释放engine对象，其保留计数器的值将在未来某个时间自动释放池被销毁时减少1.
我们呢狗狗使用简单的[engineCopy release]替代engine对象的自动释放嘛？当然可以，不过你必须在setEngine方法被调用以后再释放该engineCopy对象，否则，engineCopy对象可能在使用之前就被销毁了。采用哪种方式来管理北村取决于你自己的喜好。有些编程人员喜欢讲内存管理代码集中组织到函数中的某个方法，而有些人则喜欢在创建的位置便设置对象为自动释放对象，以免以后忘记释放这些对象。这两种方法都是有效地，不过请注意，对已iOS应用，苹果公司建议你直接使用release而不是autorelease自动释放，因为你无法知道什么时候住宿费从会释放保留计数器的值。
在carCopy创建了新的engine对象之后，copyWithZone方法执行了4次for循环，分别复制每个tire对象，并将复制的对象安装到新的car对象中。
for (int i = 0; i < 4; i++)
{
Tire *tireCopy;
tireCopy = [[self tireAtIndex: i] copy];
[tireCopy autorelease];
[carCopy setTire: tireCopy atIndex:i];
}
循环中的代码使用访问器方法在每趟循环中先后获得四个tire。然后，这些tire对象呗复制并设置为zdsf，因而他们的内存可以被正确回收。接下来，carCopy被告知在同一位置使用新的tire对象，因为我们已经在Tire类和AllweatherRadial类中精心构造了copyWithZone：方法，所以这段代码可以使用这两个类的对象正常工作。你应该注意到了，我们没有使用NSArray的copy方法，因为这样只会创建一个浅层复制而不是深层复制。
最后是完整的main（）函数，其中大多数代码你已经在前几张中见过
int main(int argc, const char * argv[]) {
@autoreleasepool{
Car *car = [[Car alloc] init];
car.name = @"Herbie";
for (int i = 0; i < 4; i++)
{
AllWeatherRadial * tire;
tire = [[AllWeatherRadial alloc] init];
[car settire: tire atIndex: i];
[tire release];
}
Slant6 *engine = [[Slant6 alloc] init];
car.engine = engine;
[engine release];
[car print];
Car *carCopy = [car copy];
[carCopy print];
[car release];
[carCopy release];
}
return (0);
} //main

改程序在输出了原始的car对象信息之后，复制了该car对象并建起内容输出。因此，我们应该得到两个完全相同的输出结果。
####13.2.4 协议和数据类型
你可以在使用的数据类型中为实例变量和方法参数指定协议名称。这样，你可以给OC的编译器提供更多的信息，从而有助于检查代码中的错误。
回想一下，id类型表示一个可以指向任何类型的对象的指针，他是一个泛型对象类型，你可以将任何对象复制给一个id类型的变量，也可以将一个id类型的变量复制给任何对象的对象指针。如果一个用尖括号括起来的惬意名称跟随在id之后，则编译器（以及阅读代码的人）江之岛你会接受任意类型的对象，但是前提是要接受该协议。
举个例子，NSControl类中有一个名为setObjectValue的方法，该方法要求对象遵守NSCopying协议。
- (void) setObjectValue: (id<NSCopying>) object;
编译器在编译该方法时，将检查参数类型，如果没有遵守协议则提出警告，如“class ‘Triangle’ does not implement the 'NSCopying' protocol”。真方便的说
