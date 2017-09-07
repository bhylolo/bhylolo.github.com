# Learning OOP

面向对象的主要设计原则
- [单一职责原则 Single Responsibility Principle](#SRP)
- [开放封闭原则 Open-Closed Principle](#OCP)
- [里斯科夫替换原则 Liskov Substitution Principle](#LSP)
- [接口隔离原则 Interface Segregation Principle](#ISP)
- [依赖倒置原则 Dependency Inversion Principle](#DIP)
- [最少知识原则 Least Knowledge Principle](#LKP)

---
<h2 id="BD">什么是“坏设计” </h2>

当一个需求变化导致程序中多个依赖模块都发生了级联的改动，那么这个程序就展现出了我们所说的 "**坏设计（bad design）**" 的特质。应用程序也相应地变得脆弱、僵化、无法预期和无法重用。

当呈现出了下述中的一个或多个特点，则就可称其为 "Bad Design"：
1. **难以修改，因为每次修改都影响系统中的多个部分。**
僵化性Rigidity体现在，如果对相互依赖严重的软件做一处改动，将会导致所有依赖的模块发生级联式的修改。当设计师或代码维护者无法预期这种级联式的修改所产生的影响时，那么这种蔓延的结果也就无法估计了。这导致软件变更的代价无法被准确的预测。
2. **当修改时，难以预期系统中哪些地方会被影响。**
脆弱性Fragility是指一处变更将破坏程序中多个位置的功能。而通常新产生的问题所涉及的模块与该变更所涉及的模块在概念上并没有直接的关联关系。这种脆弱性极大地削弱了设计与维护团队对软件的信任度。同时软件使用者和管理人员都不能预测产品的质量，因为对应用程序某一部分简单的修改导致了其他多个位置的错误，而且看起来还是完全无关的位置。而解决这些问题将可能导致更多的问题，使得维护过程陷进了 "狗咬尾巴" 的怪圈。
3. **难以在其他应用中重用，因为它不能从当前系统中解耦。**
程序员可能会被要求去调查是否能够将某个代码模块应用到不同的系统中。然而，如果设计的模块间是高度依赖的，而从一个功能模块中隔离另一个功能模块的工作量足以吓到程序员时，程序员就会放弃这种重用，因为隔离重用的代价已经高于重新设计的代价。这就是**复用性差Immobility**。

"Bad Design" 的根源————答案是：模块间的相互依赖。
如何避免、或改善Bad Design？请遵循以下方法：


---
<h2 id="SRP">单一职责原则 Single Responsibility Principle</h2>

>**一个类应该有且只有一个变化的原因。**
>*There should never be more than one reason for a class to change.*

在SRP中，**职责（Responsibility）** 定义为：一个变化的原因 *(a reason for change)*。如果你能找出多于一个动机来改变这个类的话，则这个类就包含多于一个的职责。

将不同的职责分离到不同的类中，此做法的目的是，通常每一个职责都是一个潜在的需求变化点，当需求发生变化的时候，是通过修改需求相关的类来实现的。如果一个类包含多于一个职责，则就可能会有多于一个的原因导致其发生变化。职责的过度耦合会导致设计脆弱，当需求发生变化时，容易产生无法预期的破坏。

#### 典型反例：

以下的代码是抽取自Wind框架（隐去具体代码，保留原有注释说明）：
1. 显然在 *IBaseSkyClient* 声明中包含了多余一种职责，其实它糅合进了：连接配置、链接状态、接口活动状态、压缩和加密、消息的发送和接收、业务方法等。这些方法，尤其是消息的收发和链接管理之间几乎没有交际，，他们会在程序的不同部分被调用，也会因为完全不同的原因而发生变化。当这些方法被聚合在一个类/接口中时，就是程序中本没有交集的子模块彼此依赖的根源。
2. 从产品角度来看，我们通常更习惯于将多个职责放在一起考虑，有时候为了方便起见，将多个职责放在一起实现，即当不得不实现Hub类时，采用接口的方式将不同的职责分离（接口设计遵循单一职责原则，而实现类则可能混合多个职责），以便于将上层依赖模块隔离开。这也是以下示例的第二个缺陷之处————不但实现类违反了[SRP](#SRP)，而且接口设计也违反了[ISP](#ISP)，失去了接口设计的意义。

        IBaseSkyClient = interface(IInterface)  
          ['{6384D415-3DB6-4CFC-BE7F-4983ACC50D17}']

          //Sky服务器地址列表

          //当前连接的Sky服务器地址
          //Sky服务器的端口号      
          //会话信息SessionId
          //与Sky连接认证状态
          //订阅管理列表
          //**************************Begin 代理相关属性******************************      
          //**************************End 代理相关属性********************************

          //************************Begin 重连相关属性********************************
          //**************************End 重连相关属性********************************
          //连接时间
          //最后接收数据包时间
          //最后发送数据包时间
          //支持压缩算法
          //支持的加密算法
          //加密算法种子,Byte数组
          //***************************Begin 接口方法*********************************
          //连接并登录SkyServer服务器
          //与Sky服务端断开连接
          //异步发送消息
          //增加异步消息接收事件函数，MessageEventObj为实现OnMessageEvent方法的类对象
          //主动删除异步消息接收事件函数，MessageEventObj为实现OnMessageEvent方法的类对象
          //此函数会删除所有以MessageEventObj为对象添加的异步消息接收事件函数
          //同步发送消息，ARecvMsg由上层负责实例化后传入，由底层负责对象内容的拷贝
          //发起订阅
          //取消订阅，Word数组传递进来
          //复合订阅
          //网络测试，AResponseTime单位：ms
          //对象释放
          //****************************End 接口方法**********************************

          //***************************Begin 订阅器*********************************
          //创建一个订阅器，允许创建多个，返回创建后的订阅器索引，线程安全
          //创建一个推送器，用于将获得的推送信息进行基本的反序列化
          //****************************End 订阅器**********************************

          //**************************Begin 接口事件**********************************
          //连接并通过认证的事件接口
          //连接断开的事件接口
          //通讯错误的事件接口
          //自动重连成功的事件接口
          //自动重连失败的事件接口
          //接收消息的事件接口，主连接上的消息
          //接收订阅信息的事件接口，快速连接上的订阅推送，
          //****************************End 接口事件**********************************
          //新加tcp连接超时设置       以秒为单位
          //异步发送消息   ATimeOut应答超时时间，单位为秒，最大值为60*60 (1小时)
          //如果超时，则向调用者返回一个消息，其MessageProp 值为 mpTimeout
          //如果判断消息超时后，服务端又返回了应答，则将应答抛弃，不再发给调用者
          //注意  调用者设置超时时间的大小要掌握好  无应答的消息发送不要调用此函数
          //批量更新,必须确保成对出现
        end;  
---
<h2 id="OCP">开放封闭原则 Open-Closed Principle</h2>

> **所有系统在其生命周期中都会发生变化，只要系统要开发一个版本以上这一点就需时刻记住。**
*All systems change during their life cycles. This must be borne in mind when developing systems expected to last longer than the first version. — Ivar Jacobson*

> **软件实体（类、模块、函数等）应对扩展开放，但对修改封闭。** *Software entities (classes, modules, functions, etc.) should be open for extension, but closed for modification.* ——Bertrand Meyer 

这段话就是是Bertrand Meyer在1988年提出的**开放封闭原则**，用以解释“*如何才能构建一个稳定的设计来面对变化*”。

#### 如何理解“开放封闭原则”
符合开放封闭原则的模块都有两个主要特性：
1. **它们 "面向扩展开放（Open For Extension）"。**
也就是说模块的行为是能够被扩展的。当应用程序的需求变化时，我们可以使模块表现出全新的或与以往不同的行为，以满足新的需求。
2. **它们 "面向修改封闭（Closed For Modification）"。**
模块的源代码是不能被侵犯的，任何人都不允许修改已有源代码。

看起来上述两个特性是互相冲突的，因为通常扩展模块行为的常规方式就是修改该模块。一个不能被修改的模块通常被认为其拥有着固定的行为。那么如何使这两个相反的特性共存呢？
> **抽象是关键** Abstraction is the Key.

在使用面向对象设计技术时，可以创建固定的抽象，这里的抽象指的是抽象基类，这时，外部模块依赖于一个固定的抽象，然后该抽象模块的行为可以通过创建衍生类来扩展。

> **注意**：完全闭合是不现实的，程序设计师必须甄别其设计对哪些变化封闭。
---
<h2 id="LSP">里斯科夫替换原则 Liskov Substitution Principle</h2>

**开放封闭原则**强调设计良好的代码可以不通过修改而扩展，新的功能通过添加新的代码来实现，而不需要更改已有的可工作的代码。抽象（Abstraction）和多态（Polymorphism）是实现这一原则的主要机制，而继承（Inheritance）则是实现抽象和多态的主要方法。那么是什么设计规则在保证对继承的使用呢？

**里斯科夫替换原则（LSP: The Liskov Substitution Principle）**
使用基类对象指针或引用的函数必须能够在不了解衍生类的条件下使用衍生类的对象
*Functions that use pointers or references to base classes must be able to use objects of derived classes without knowing it.* 


![Liskov](http://img.bimg.126.net/photo/RoRHknagVWDv6DCyQkIyNw==/606297099836152405.jpg)

>Barbara Liskov 在 1988 年提出了这一原则：
>
>What is wanted here is something like the following substitution property: If for each object o1 of type S there is an object o2 of type T such that for all programs P defined in terms of T, the behavior of P is unchanged when o1 is substituted for o2 then S is a subtype of T.

也就是说，所有的衍生类必须符合使用者所期待的基类的行为。

---
<h2 id="ISP">接口隔离原则 Interface Segregation Principle</h2>

> **客户类不应被强迫依赖那些它们不需要的接口** 
*Clients should not be forced to depend upon interfaces that they do not use.*

**接口分离原则（Interface Segregation Principle）** 用于处理胖接口（fat interface）所带来的问题。如果类的接口定义暴露了过多的行为，则说明这个类的接口定义内聚程度不够好。换句话说，类的接口可以被分解为多组功能函数的组合，每一组都服务于不同的客户类，而不同的客户类可以选择使用不同的功能分组。

ISP 原则承认了对象设计中非内聚接口的存在。但它建议客户类不应该只通过一个单独的类来使用这些接口。取而代之的是，客户类应该通过不同的抽象基类来使用那些内聚的接口。在不同的编程语言中，这里所指的抽象基类可以指 "接口（interface）"、"协议（protocol）"、"签名（signature）" 等。

<h2 id="DIP">依赖倒置原则 Dependency Inversion Principle</h2>

> **依赖倒置原则**

>A. 高层模块不应该依赖于低层模块，二者都应该依赖于抽象。High level modules should not depend upon low level modules. Both should depend upon abstractions.

>B. 抽象不应该依赖于具体实现细节，而具体实现细节应该依赖于抽象。Abstractions should not depend upon details. Details should depend upon abstraction.

#### 分层（Layering）

依据 Grady Booch 的定义：

> **所有结构良好的面向对象架构都有着清晰明确的层级定义，每一层都通过一个定义良好和可控的接口来提供一组内聚的服务集合。** 
*All well-structured object-oriented architectures have clearly-defined layers, with each layer providing some coherent set of services though a well-defined and controlled interface.*

---
<h2 id="LKP">最少知识原则 Least Knowledge Principle</h2>

**最少知识原则（Least Knowledge Principle），或者称迪米特法则（Law of Demeter）**，是一种面向对象程序设计的指导原则，它描述了一种保持代码松耦合的策略。其可简单的归纳为：
> 每个单元对其他单元只拥有有限的知识，只了解与当前单元紧密联系的单元 Each unit should have only limited knowledge about other units: only units "closely" related to the current unit.

再简洁些：
> 每个单元只能和它的 "朋友" 交谈，不能和 "陌生人" 交谈 
Each unit should only talk to its friends; don't talk to strangers.

更简洁些：
> 只和自己直接的 "朋友" 交谈。
Only talk to your immediate friends.

应用到面向对象的程序设计中时，可描述为 
> "类应该与其协作类进行交互但无需了解它们的内部结构"。
A class should interact directly with its collaborators and be shielded from understanding their internal structure.

迪米特法则（Law of Demeter）由 Northeastern University 的 Ian Holland 在 1987 年提出，"Law of Demeter" 名称是来自当时正在进行的一项研究 "The Demeter Project"。

改进后的原则称为 LoDC（Law of Demeter for Concerns），它为软件设计带来了两个主要的益处：

> 更好的信息隐藏和更少的信息重载。
It leads to better information hiding.
It leads to less information overload.

具体点儿就是，对象应尽可能地避免调用由另一个方法返回的对象的方法。

#### 应用最少知识原则优点和缺点

- 优点：遵守 Law of Demeter 将降低模块间的耦合，提升了软件的可维护性和可重用性。
- 缺点：应用 Law of Demeter 可能会导致不得不在类中设计出很多用于中转的包装方法（Wrapper Method），这会提升类设计的复杂度。

