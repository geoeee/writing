#####8.5.5 NSDictionary
你肯定听说过字典，也许偶尔还会用到它。在编程中，字典（dictionary）是关键字及其定义的集合。Cocoa中有一个实现这种功能的集合类NSDictionary。NSDictionary能在给定的关键字（通常是一个NSString字符串）下存储一个数值（可以使任何类型的OC对象），然后你就可以用这个关键字来查找相应的数据。因此加入你有一个存储了某人所有联系方式的NSDictionary，那么你可以对这个字典说“给我关键字home-address下的值”或者“给我关键字emial-address下的值”。
说明：为什么不用数组存储然后再数组里查询数值呢？字典（也被称为散列表或关联数组）使用的是键查询的优化方式。它可以立即找出要查询的数组，而不需要遍历整个数组。对于频繁查询和大型的数据集来说，使用字典比数组要快得多。
你可能已经猜到，NSDictionary就像NSString和NSArray一样是不可变的对象。但是NSMutableDictionary类允许你添加和删除字典元素。在创建新的NSDictionary时，就要提供该字典所存储的全部对象和关键字。
学习字典的最简单方法就是用字典字面量语法，它与类方法dictionaryWithObjectsAndKeys：非常相似。
字面量语法即使用类似@{key：value，...}的方法来定义。需要注意它使用的是大括号而不是方括号。还要注意dictionaryWithObjectsAndKeys:后面的参数先是要存心湖的对象，然后才是关键字，而字面量的语法则是关键字在前，数值在后。关键字和数值之间以冒号分开，而每对键值之间则用逗号区分开。
+ (id) dictionaryWithObjectsAndKeys: (id) firstObject,...;
该方法接受对象和关键字交替出现的序列，以nil值作为终止符号（你可能已经猜到了，NSDictionary中不能存储nil值，而且字面量语法中也不需要nil值）。假设我们想创建一个存储汽车轮胎的字典，而轮胎要使用可读的表钱进行查找而不是数组中的任意索引。你可以按下面的方式创建这种字典。
Tire *t1 = [Tire new];
Tire *t2 = [Tire new];
Tire *t3 = [Tire new];
Tire *t4 = [Tire new];
NSDictionary *tires = [NSDictionary dictionaryWithObjectsAndKeys: t1,
@"front-left", t2, @"front-right", t3, @"back-left", t4, @"back-right", nil];
也可以使用
NSDictionary *tires = @{@"front-left" : t1, @"front-right" : t2, @"back-left" : t3,
@"back-right" : t4};
使用objectForKey：方法来传递前面用来存储的关键字，就可以访问字典中的数值了。
- （id） objectForKey： （id） aKey;
或者是
tires[key];
所以假设你要查找右后方的轮胎，可以这样写
Tire *tire = [tires objectForKey: @"back-right"];
或者是
Tire *tire = tires[@"back-right"];
如果字典里没有右后方的轮胎（假设他是一辆只有三个轮子的奇怪汽车），字典将会返回nil值。
向NSMutableDictionary类发送dictionary消息，便可以创建新的NSMutableDictionary对象，也可以使用dictionaryWithCapacity：方法来创建新的可变字典并告诉Cocoa该字典的最终大小（有没有发现Cocoa的命名方式很有规律？）
+ (id) dictionaryWithCapacity: (NSUInteger)numItems;
与之前提到过的NSMutableString和NSMutableArray一样，在这里，字典的容量也仅仅是一个建议，而不是对其大小的先知。你可以使用setObject：forKey：方法为字典添加元素；
- （void） setObject:(id)anObject forKey:(id)aKey;
存储轮胎的字典还有一种创建方法：
NSMutableDictionary *tires = [NSMutableDictionary dictionary];
[tires setObject:t1 forKey:@"front-left"];
[tires setObject:t2 forKey:@"front-right"];
[tires setObject:t3 forKey:@"back-left"];
[tires setObject:t4 forKey:@"back-right"];
如果对字典中已有的关键字使用setObject：forKey：方法，那么这个方法会将新值替换掉原来的数值。如果想在可变字典中删除一些关键字，可使用removeObjectForKey：方法
- （void） removeObjectForKey: (id) aKey;
所以，如果想模拟一只轮胎脱落的长江，就可以把那只轮胎从字典中删除。
[tires removeObjectForKey:@"back-left"];
与NSArray一样，没有适用于NSMutableDictionary的字面量初始化语法。
#####8.5.6 请不要乱来
如果你很有创造力，并跃跃欲试地想要创建NSString，NSArray或者NSDictionary的子类，请不要这么做。在一些语言中，你确实会用到字符串和数组的子类来完成工作，但是在Cocoa中，许多类实际上是以类簇（class clusters）的方式实现的，即他们是一群隐藏在通用接口之下的与实现相关的类。创建NSString对象时，实际上获得的可能是NSLiteralString，NSCFString，NSSimpleString，NSBallOfString或者其他没有写入文档的与实现相关的对象。
程序员在使用NSString和NSArray时不必在意系统内部到底用的是那个类，但要给一个类簇穿件子类是一件灵纹十分痛苦的事情。通常，可以将NSString和NSArray符合到你的某个类中，或者使用类别（将在第12章中介绍）来解决这种编程中的问题，而不用特意去创建子类。
