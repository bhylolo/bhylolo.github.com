# 理解和使用DELPHI

## 一些语法特性

### 结构体
Delphi中的结构体关键字为record。Record分为经典的结构体（traditional record）和扩展结构体（advanced record）
如何构造RECORD
自动构造/析构
NEW/DISPOSE
结构体一定比对象效率高吗？
### RTTI(运行时类型信息)
#### PUBLISHED的特性
Published和Public具有相同的可见性（visibility rule），但在相关编译指令打开的时，published区域（section）中的field、property、event、method等元信息（metadata）会被编译进最终的binary程序中，这些元信息称之为RTTI
运行时类型信息（RTTI RUN-TIME TYPE INFORMATION）
RTTI是一种在运行时可以动态获得类型信息（metadata）的编程范式，针对这种形式的编程称为元编程。当相关编译指令打开时，类型的元信息会被写入到最终编译的程序（binary）中。能够被写入到RTTI的类型包括：class、field、property、method、attribute。
虽然RTTI提供了元编程的能力，但由于将额外的metadata编译进binary，也会使得binary体积变大。此外，在跨语言、跨dll调用时，需要小心作用域（domain）。但另一方面，这种技术又经常被用在跨作用域（domain）调用、传递数据中，它能够使得序列化变得更简单，从而做出更通用的框架。
控制RTTI的编译指令
编译指令	默认值	说明
$M
$TYPEINFO	{$M-}
{$TYPEINFO OFF}	$M 用以控制是否将class的property、event写入到RTTI中，如果class声明了$M，或者父类声明了$M，则published的property、event会被写入到RTTI中。

$M 也可以作用于接口interface，将其所有的property将被写入RTTI

由于TPersistent声明了$M，所以从TPersistent继承的class也都能将published的属性写入到RTTI中。
$METHODINFO	{$METHODINFO OFF}	只有在{$TYPEINFO ON }时，该编译指令才会起作用，用以将published中的method的参数信息写入到RTTI中。

当{$METHODINFO OFF }时，RTTI中只会记录函数名，而不会记录参数信息。

$METHODINFO在VCL中最主要的用途是用于SOAP的实现，详见 Invokable Interface
{$RTTI}		该编译指令的目的是给程序员拥有更多的选择权，来控制对RTTI以更细节的控制，通过参数INHERIT和EXPLICIT来控制只继承、或不继承部分property、method的RTTI
$WEAKLINKRTTI	{ $WEAKLINKRTTI  OFF}	在默认情况下，当$RTTI打开时，针对相应的method要么全部编译进binary，要么全部都不编译进binary，$WEAKLINKRTTI则用来控制有多少RTTI最终被编译进binary
访问RTTI
RTTI与元编程
虚方法拦截器
类似于.NET和Java中的动态代理，使用TvirtualMethodInterceptor可以在运行时对class的所有虚方法在调用前、调用后进行拦截，从而在一定程度上，可以在Delphi上使用AOP。
uses
  SysUtils, Rtti;
 
type
  TFoo = class
    // Frob doubles x and returns the new x + 10
    function Frob(var x: Integer): Integer; virtual;
  end;
 
function TFoo.Frob(var x: Integer): Integer;
begin
  x := x * 2;
  Result := x + 10;
end;
 
procedure WorkWithFoo(Foo: TFoo);
var
  a, b: Integer;
begin
  a := 10;
  Writeln('  [WorkWithFoo] before: a = ', a);
  try
    b := Foo.Frob(a);
    Writeln('  [WorkWithFoo] Result = ', b);
    Writeln('  [WorkWithFoo] after:  a = ', a);
  except
    on e: Exception do
      Writeln('  Exception: ', e.ClassName);
  end;
end;
 
procedure P;
var
  foo: TFoo;
  vmi: TVirtualMethodInterceptor;
begin
  vmi := nil;
  foo := TFoo.Create;
  try
    Writeln('Before hackery:');
    WorkWithFoo(foo);
 
    vmi := TVirtualMethodInterceptor.Create(foo.ClassType);
 
    vmi.OnBefore := procedure(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue)
    var
      i: Integer;
    begin
      Write('[OnBefore] Calling ', Method.Name, ' with args: ');
      for i := 0 to Length(Args) - 1 do
        Write(Args[i].ToString, ' ');
      Writeln;
    end;
 
    // Change foo's metaclass pointer to our new dynamically derived
    // and intercepted descendant
    vmi.Proxify(foo);
 
    Writeln('After interception:');
    WorkWithFoo(foo);
  finally
    foo.Free;
    vmi.Free;
  end;
end;
 
begin
  P;
  readln; // To see what's in console before it goes away.
end.

动态执行函数
以下代码示例展示了如何动态调用TSomeClass.SomeMethod(String, Integer) 
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  { Create a new RTTI context }
  LContext := TRttiContext.Create();
  { Obtain the TRttiType for a given TSomeClass class }
  LType := LContext.GetType(TSomeClass);
  { Invoke the method called SomeMethod in the class }
  LType.GetMethod('SomeMethod').Invoke(['FirstParameter', 2]);
end;

匿名函数
作用域
实现ITERATOR模式
从TOBJECT，TPERSISTENT，TCOMPONENT，TCOLLECTION/TCOLLECTIONITEM 继承有什么不同
如何使用INTERFACE
INTERFACE的作用
使用INTERFACE实现注册中心
通常会有一种需求是，希望实现一个注册中心来解绑框架与各个package之间的依赖。
如何看待使用引用计数来自动管理对象的生命周期
IINTERFACE 和 IDISPATCH
使用可视化组件库（VCL）
对象的序列化
可序列化的对象和属性
定制序列化的内容
常用的序列化相关函数
TCLIENTDATASET的一些使用技巧
使用数据感知控件
创建结构
增删改查
遍历
当DataSet和数据感知UI控件绑定时，为了提高遍历或查询性能，需要在解除/重新绑定游标滚动时间 
ADT/ARRAY FIELD
FRAME
Frame是一种用于设计器的控件容器
DATAMOUDLE

DELPHI的多线程编程
THREAD对象
SYNCHRONIZED 方法

TINTERFACELIST
IDE 使用技巧
建立UML工程
快捷键
快捷键	功能
Ctrl+D	格式化代码
Ctrl+J	代码模板
Ctrl+Y	删除当前行
Ctrl+Shift+U/I	往左/右缩进一个Tab
Ctrl+F12	打开project中的unit
Shift+F2	打开project中的Form
Alt+F11	添加工程中的unit到当前单元implements 下的uses
Ctrl+F11	将文件添加到当前project中
Shift+F11	打开文件（不一定是Project中的unit）

代码模板
在IDE中，为了提高输入代码的效率，通常会将一些常用的代码结构设置成模板，通过敲击“别名（alias）”+快捷键来输入。
快捷键
<快捷代码>+Ctrl+J

如何创建代码模板