# Learning Delphi X
<h2 id="context">Delphi语法</h2>

* [语言概述](#language )
* [程序和单元](#units)
* [基础语法元素](#syntactic)
* [数据类型、变量和常量](#datatype)
* [过程和函数](#procedure)
* [类和对象](#class)
* [标准函数和输入输出函数](#routine)
* [库和包](#library)
* [对象接口](#interface)
* [内存管理](#memory)
* [程序控制](#control)
* [内联汇编代码（只针对windows）](#assembly)
* [泛型](#generic)
* [属性（Attributes）运行时类型信息RTTI](#attribute)

<h3 id="language" />

### [语言概述](#context)
----
Delphi是一种编译型、强类型的高级语言。Delphi支持结构化和面向对象程序设计。语法基于Object Pascal，这是一种代码容易阅读，编译更块，支持多文件模块化编程的语言。

#### 程序的组织

Delphi程序的源码文件称为：unit（单元）。一般，Delphi程序源码的入口是关键字`program`，是以`.dpr`为后缀的文件。dpr文件的布局是：

    //程序以 program 作为入口第一个关键字，紧随其后的是程序的名字
    program Name;

    //uses 列表是可选的，其中列出了该程序所有依赖的unit，这些unit可能是公共单元，也可能是只针对该程序的
    uses
      Vcl.Form,
      uMyUnit in 'uMyUnit.pas'

    //这里可以声明类型、函数、变量等
    // …… 声明 ……
    
    begin
        //这里是语句。
    end.

由于`uses`列表为编译器指定了依赖文件的信息，所以与其他语言不一样，Delphi不再额外需要makefile、header头文件、以及预处理指令`include`。

**Delphi的源文件**

Delphi编译器只会编译从以下三种文件中编译源码：

* 单元文件`Unit`(`.pas`后缀的文件) 
* 工程文件`Project`(`.dpr`后缀的文件）
* 包文件`Package`(`.dpk`后缀的文件) 

单元文件`Unit`中包含了程序的源码，每一个程序的源文件工程中只能有一个`.dpr`工程文件，但可以有很多`.pas`单元文件。工程文件是基于传统的`pascal`语法，记录了工程所需的所有单元文件。工程文件通常都是由IDE自动维护。

如果是使用命令行工具来编译程序，可以不需要dpr文件，而直接指定和编译单元文件（`.pas`），如果是使用IDE来编译的话，则一定要指定一个工程文件（`.dpr`）。

包文件`Package`中的代码内容类似于工程文件`project`，只是它是用来编译生成一种特殊的动态链接库，叫package包。

**程序编译过程中需要的其他文件** 

除了代码文件之外，IDE为了能够编译工程，还生成了一些非pascal的文件，这些文件通常也是由IDE自动维护，它们包括:

* VCL form 窗体文件(Win32平台上.dfm后缀的文件) 
* 资源文件(.res后缀的文件) 
* 工程选项文件(.dof后缀的文件) 

一个VCL 窗体文件中包含了所对应的窗体`form`或控件`component`的属性值。每一个窗体文件都对应一个窗体（windows窗体或者dialog）。IDE以文本格式加载和编辑窗体文件，再保存成文本格式或二进制（通常文本格式更利于版本控制）。尽管dfm（窗体文件）是存成文本格式，但是一般不需要文件进行直接修改，而是使用IDE的可视化设计器来操作。每一个Application工程都至少含有一个form（窗体），每一个form都对应一个pas文件，pas文件同dfm文件同名，只是后缀不同。

除了VCL form文件，每一个工程都有一个资源文件（.res）来存储application的icon，以及其他资源，例如字符串等。默认情况，这些资源文件的名称同工程文件（.dpr）同名。

工程选项文件（.dof）中间保存了编译和链接的设置、搜索路径、版本信息等等。每一个工程文件（.dpr）都关联一个同名的.dof文件。通常，选项文件（.dof）的内容是通过Project Option（工程选项）来设置的。

IDE中其他各种工具也会将设置存成其他后缀的文件，例如桌面设置（Desktop setting）存成.dsk，里面保存了IDE窗体设置等一些信息，桌面设置可以是由当前打开的工程来指定，也可以采用IDE关键设置。但这些设置文件都不会影响到程序的编译。

**同编译器相关联的文件**

当第一次编译一个application（程序）或者package（包）时，工程中每一个新.pas文件（新创建或修改过的pas文件），编译器都会生成一个对应的.dcu文件，所有的.dcu文件随后会被链接创建成一个exe可执行文件或一个package。在编译package时，编译器一样会对package中每一个新.pas文件编译生成对应的.dcu文件，同时还会针对package生成.dcp和package包文件。如果打开GD编译开关的话，连接器还会生成map文件和.dcr文件（.dcr文件中包含了字符串资源，这些资源可以被编译进一个资源文件）。

当一个工程在被编译的时候，只有在以下情况，各个单元文件才会被编重新译，否则不会再次编译单元文件：
- 在最后一次编译后，单元文件被修改过。
- 单元文件对应的.dcu/dpu文件不存在
- 你明确告知编译器需要重新编译工程
- 单元所依赖的外部接口、单元发生改变。

事实上，以上并不是判断一个单元是否需要被重新编译的必要条件，对于编译器来说，只要发现一个已经编译过的单元，它所依赖的单元都没有发生变化，则就不会重新编译该单元。

#### 程序（Program）示例

以下的栗例子将展示Delphi程序的一些基本功能。这些简单的示例，通常不是使用IDE编译的，而是使用命令行编译的。

**一个简单的控制台程序（Console Application）**

下面这个是一个简单的控制台程序（console application）你可以在命令行中编译、执行。 

    program Greeting;
    
    {$APPTYPE CONSOLE}
    
    var
      MyMessage: string;
    
    begin
      MyMessage := 'Hello world!';
      Writeln(MyMessage);
    end.

第一行声明了这个程序名字叫Greeting。随后，编译指令{$APPTYPE CONSOLE}告诉编译器，这是一个控制台程序，是在命令行中运行的。再往下，是声明一个变量叫MyMessage，它是string类型（Delphi的string类型的数据是被自动管理的）。后面的语句是将字符串"Hello world!"赋值给变量MyMessage，将MyMessage变量 的内容发送给标准输出函数Writeln。（Writeln函数被定义在System单元中，由编译器自动包含到每一个单元的引用之中。）

你可以将整段代码写到文件中，文件命名为`greeting.pas`或者`greeting.dpr`，然后使用以下的命令编译这个文件： 

`dcc32 greeting` 

编译成功后，即产生一个Win32的可执行文件。

The resulting executable prints the message `Hello world!` 

Aside from its simplicity, this example differs in several important ways from programs that you are likely to write with Embarcadero development tools. First, it is a console application. Embarcadero development tools are most often used to write applications with graphical interfaces; hence, you would not ordinarily call Writeln. Moreover, the entire example program (save for Writeln) is in a single file. In a typical GUI application, the program heading the first line of the example would be placed in a separate project file that would not contain any of the actual application logic, other than a few calls to routines defined in unit files. 

##A More Complicated Example## 

The next example shows a program that is divided into two files: a project file and a unit file. The project file, which you can save as `greeting.dpr`, looks like this: 

    program Greeting;
    
    {$APPTYPE CONSOLE}
    
    uses
      Unit1;
    
    begin
      PrintMessage('Hello World!');
    end.
 
 The first line declares a program called `greeting`, which, once again, is a console application. The `uses Unit1;` clause tells the compiler that the program `greeting` depends on a unit called `Unit1`. Finally, the program calls the `PrintMessage` procedure, passing to it the string `Hello World!` The `PrintMessage` procedure is defined in Unit1. Here is the source code for `Unit1`, which must be saved in a file called `Unit1.pas`: 

    unit Unit1;
    
    interface
    
    procedure PrintMessage(msg: string);
    
    implementation
    
    procedure PrintMessage(msg: string);
    begin
        Writeln(msg);
    end;
    
    end.
 
 `Unit1` defines a procedure called `PrintMessage` that takes a single string as an argument and sends the string to the standard output. (In Delphi, routines that do not return a value are called `procedures`. Routines that return a value are called `functions`.) 

Notice that `PrintMessage` is declared twice in `Unit1`. The first declaration, under the reserved word interface, makes `PrintMessage` available to other modules (such as greeting) that use `Unit1`. The second declaration, under the reserved word `implementation`, actually defines `PrintMessage`. 

You can now compile `Greeting` from the command line by entering 

    dcc32 greeting 

to produce a Win32 executable. 

There is no need to include `Unit1` as a command-line argument. When the compiler processes `greeting.dpr`, it automatically looks for unit files that the `greeting` program depends on. The resulting executable does the same thing as our first example: it prints the message `Hello world!` 

**A VCL Application** 

Our next example is an application built using the Visual Component Library (VCL) components in the IDE. This program uses automatically generated form and resource files, so you won't be able to compile it from the source code alone. But it illustrates important features of the Delphi Language. In addition to multiple units, the program uses [classes and objects](#class). 

The program includes a project file and two new unit files. First, the project file: 

    program Greeting;
    
    uses
    Forms, Unit1, Unit2;
    
    {$R *.res} { This directive links the project's resource file. }
    
    begin
        { Calls to global Application instance }
        Application.Initialize;
        Application.CreateForm(TForm1, Form1);
        Application.CreateForm(TForm2, Form2);
        Application.Run;
    end.
 
 Once again, our program is called `greeting`. It uses three units: `Forms`, which is part of VCL; `Unit1`, which is associated with the application's main form (`Form1`); and `Unit2`, which is associated with another form (`Form2`). 

The program makes a series of calls to an object named `Application`, which is an instance of the `Vcl.Forms.TApplication` class defined in the Forms unit. (Every project has an automatically generated `Application` object.) Two of these calls invoke a `Vcl.Forms.TApplication` method named `CreateForm`. The first call to `CreateForm` creates `Form1`, an instance of the `TForm1` class defined in `Unit1`. The second call to `CreateForm` creates `Form2`, an instance of the `TForm2` class defined in `Unit2`. 

`Unit1` looks like this: 

    unit Unit1;
    
    interface
    
    uses SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs;
    
    type
    TForm1 = class(TForm)
        Button1: TButton;
        procedure Button1Click(Sender: TObject);
    end;
    
    var
    Form1: TForm1;
    
    implementation
    
    uses Unit2;
    
    {$R *.dfm}
    
    procedure TForm1.Button1Click(Sender: TObject);
    begin
        Form2.ShowModal;
    end;
    
    end.
 
 `Unit1` creates a class named `TForm1` (derived from `Vcl.Forms.TForm`) and an instance of this class `Form1`. The `TForm1` class includes a button -- `Button1`, an instance of `Vcl.StdCtrls.TButton` -- and a procedure named `Button1Click` that is called when the user presses `Button1. Button1Click` hides `Form1` and displays `Form2` (the call to `Form2.ShowModal`). 

Note: In the previous example, `Form2.ShowModal` relies on the use of auto-created forms. While this is fine for example code, using auto-created forms is actively discouraged.
`Form2` is defined in `Unit2`: 

    unit Unit2;
    
    interface
    
    uses SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs;
    
    type
      TForm2 = class(TForm)
        Label1: TLabel;
        CancelButton: TButton;
        procedure CancelButtonClick(Sender: TObject);
    end;
    
    var
      Form2: TForm2;
    
    implementation
    
    uses Unit1;
    
    {$R *.dfm}
    
    procedure TForm2.CancelButtonClick(Sender: TObject);
    begin
        Form2.Close;
    end;
    
    end.
 
 `Unit2` creates a class named `TForm2` and an instance of this class, `Form2`. The `TForm2` class includes a button (`CancelButton`, an instance of `Vcl.StdCtrls.TButton`) and a label (`Label1`, an instance of `Vcl.StdCtrls.TLabel`). You can not see this from the source code, but `Label1` displays a caption that reads `Hello world!` The caption is defined in `Form2`'s form file, `Unit2.dfm`. 

`TForm2` declares and defines a method `CancelButtonClick` that will be invoked at run time whenever the user presses `CancelButton`. This procedure (along with `Unit1`'s `TForm1.Button1Click`) is called an event handler because it responds to events that occur while the program is running. Event handlers are assigned to specific events by the form files for `Form1` and `Form2`. 

When the `greeting` program starts, `Form1` is displayed and `Form2` is invisible. (By default, only the first form created in the project file is visible at run time. This is called the project's main form.) When the user presses the button on `Form1`, `Form2` displays the `Hello world!` greeting. When the user presses the `CancelButton` or the `Close` button on the title bar, `Form2` closes. 



<h3 id="units" />

### 程序和单元

<h3 id="syntactic" />

### 基础语法元素

<h3 id="datatype" />

### 数据类型、变量和常量

<h3 id="procedure" />

### [过程和函数](#context)
---
#### 函数的声明
函数,在Delphi相关的文档中,英文的表述为function\procedure\routine,除了一些微小的区别,基本上表述的都是 **函数**
- function: 指有返回值的函数,声明形式为 **function FunName(Param:Type): Type;**
- procedure: 指没有返回值的函数,声明形式为 **procedure FunName(Param: Type);** 
- routine: 是对function和procedure的统称

**声明示例**

    function Max(A: array of Real): Real;
    var
        X: Real;
        I: Integer;
    begin
        if Length(A)=0 then
            Exit(0); //Exit忽略之后一直到end之间所有的函数代码,跳出当前函数,函数的返回值为()中间的数值
        X := A[0];
        for I := Low(A) to High(A) do
            if X < A[I] then X := A[I];
        Result:= X; //函数的返回值也可以表示成 Max := X;
    end;

    procedure NumString(N: Integer; var S: string);
    var
       V: Integer;
     begin
       V := Abs(N);
       S := '';
       repeat
         S := Chr(V mod 10 + Ord('0')) + S;
         V := V div 10;
       until V = 0;
       if N < 0 then S := '-' + S;
     end;

**重载overload**

    ... ... 
    procedure Store(X: Longint); overload;    
    procedure Store(X: Shortint); overload;    

    implementation

    function Divide(X, Y: Real): Real; overload;
    begin
        Result := X/Y;
    end
    
    function Divide(X, Y: Integer): Integer; overload;
    begin
       Result := X div Y;
    end;

**前向声明**

    function Calculate(X, Y: Integer): Real; forward;

当声明的函数需要作为DLL的输出函数,或者COM等接口函数供外部程序集(有可能时跨语言)调用,一般需要明确指定函数的调用规则*Calling Convention*,声明示例:

    function MyFunction(X, Y: Real): Real; cdecl;

**调用规则**, 在Delphi中Calling Convention的类型如下:

|声明|参数调用顺序|参数清理者|用寄存器传参|说明|
|--|--|--|--|--|
|register  |Undefined  |Routine  | Yes |使用CPU寄存器传参,除此之外,其他的调用规则都是直接在stack上传递参数|
|pascal |Undefined |Routine |No |
|cdecl |Right-to-left |Caller |No |参数调用顺序等同于C语言的顺序,如果作为DLL输出函数,需要被C语言调用的话,则在Delphi中声明的函数需要使用 cdel或stdcall. 但不同于其他规则,只有cdel规则是调用者caller负责在stack删除参数,其他规则,都是函数自身负责删除参数.通常,推荐使用stdcall,相对来说效率会好于cdel
|stdcall |Right-to-left |Routine |No 
|safecall |Right-to-left |Routine |No |该调用规则需要实现exception firewall异常防火墙.在Win32平台上,是由COM error notification来实现的.
 
 **外部函数**

 如果函数的实现部分不在当前的程序域中,则函数的声明部分需要用指示字注明
 - 当需要调用的函数是在单独的object file中间,需要使用编译指示字注明链接 object file

        {$L BLOCK.OBJ}
        procedure MoveWord(var Source, Dest; Count: Integer); external;

- 如果函是外部DLL中间的函数的话,则还需要注明DLL名称和在DLL中的函数名

        function MessageBox(HWnd: Integer; Text, Caption: PChar; Flags: Integer): Integer;stdcall; external 'user32.dll' name 'MessageBoxA';

        // 也可以使用序号index来替代DLL中输出函数的名称,例如: external stringConstant index integerConstant;

**内嵌函数声明:**

    procedure DoSomething(S: string);
    var
        X, Y: Integer;
    
    procedure NestedProc(S: string);
    begin
      ...
    end;
    
    begin
        ...
        NestedProc(S);
        ...
    end;

---
#### 函数的参数
函数的参数声明位置在函数名之后,参数的形式举例如下:

    (X, Y: Real)
    (var S: string; X: Integer)
    (HWnd: Integer; Text, Caption: PChar; Flags: Integer)
    (const P; I: Integer)

如果函数没有参数的话,则可以没有(),例如

    procedure UpdateRecords;
    begin
        ...
    end;

**参数的传值和传递引用** *Value and Variable Parameters* 

    function DoubleByValue(X: Integer): Integer;   // X is a value parameter
    begin
        X := X * 2;
        Result := X;
    end;
    
    function DoubleByRef(var X: Integer): Integer;  // X is a variable parameter
    begin
        X := X * 2;
        Result := X;
    end;

**Const**

声明为const是read-only的参数,且是传递应用的,类似**var**

    function CompareStr(const S1, S2: string): Integer;

**Out**

out也是传递引用,但与var不同的是,out只是为了传出而用的,所以不要在传给函数之前,在外部对于out参数做任何赋值或初始化,因为它会被函数内部初始化.

    procedure GetInfo(out Info: SomeRecordType);

**Untyped**

当函数参数指示为var/const/out 时,可以不指定具体的类型,在函数内部访问该参数的时候,可以强制转换为特定类型使用,或者转换为variant类型使用.

    procedure TakeAnything1(const C);
    procedure TakeAnything2(var C);
    procedure TakeAnything3(out C);

**Array / Open Array**

定长数组参数的声明

    type TDigits = array[1..10] of Integer;
    procedure Sort(A: TDigits);

*不能声明成这样:*

    procedure Sort(A: array[1..10] of Integer)  // syntax error

Open Array 开放数组(不定长)的声明

    function Find(A: array of Char): Integer;

也可以是这样声明

    type
        TDynamicArray = array of Integer;
        procedure p(Value: TDynamicArray);

开放数组的调用 

    procedure PrintArray(A: array of Real);
    var
        I: Integer;
    begin
        for I := 0 to High(A) do 
            Writeln(A[I]);
    end;

    var
        LArr: array [0..3] of Real;
    begin
        LArr[0]:= 0;
        LArr[1]:= 1;
        LArr[2]:= 2;
        PrintArray(LArr);

        PrintArray([1.1., 2.2. ,3.3, 4.4]);
    end.

Variant 开放数组

    function MakeStr(const Args: array of const): string;
    var
        I: Integer;
    begin
        Result := '';
        for I := 0 to High(Args) do
            with Args[I] do
                case VType of
                vtInteger:  Result := Result + IntToStr(VInteger);
                vtBoolean:  Result := Result + BoolToStr(VBoolean);
                vtChar:     Result := Result + VChar;
                vtExtended: Result := Result + FloatToStr(VExtended^);
                vtString:   Result := Result + VString^;
                vtPChar:    Result := Result + VPChar;
                vtObject:   Result := Result + VObject.ClassName;
                vtClass:    Result := Result + VClass.ClassName;
                vtAnsiString:  Result := Result + string(VAnsiString);
                vtUnicodeString:  Result := Result + string(VUnicodeString);
                vtCurrency:    Result := Result + CurrToStr(VCurrency^);
                vtVariant:     Result := Result + string(VVariant^);
                vtInt64:       Result := Result + IntToStr(VInt64^);
            end;
    end;    s

    begin   
        MakeStr(['test', 100, ' ', True, 3.14159, TForm]);
    end.
                                                                                     
**参数的默认值**

    function Add(X: Real = 3.5; Y: Real = 3.5): Real;
    begin
        result:= X+Y;
    end;

    begin
        writeln(Add());
        writeln(Add(1, 2));
    end.

---
#### 函数类型

函数也可以作为一种类型来声明,这样就可以指定一个变量的类型为函数类型.注意体会以下区别:

*声明函数*

    function Add(X, Y: real): real;    

    implementation

    function Add(X, Y: real):real;
    begin
        result:= X + Y;
    end;

*声明函数类型*

    type
        TFun = function (X, Y: real): real; // 普通函数
        TObjFun = function (X, Y: real): real of object; // 类函数
        TAnFun = reference to function (X, Y: real): real; //匿名函数

*普通函数类型的使用*

    function Add(X, Y: real): real;
    begin   
        result:= X+Y;
    end;

    procedure CalAdd();
    var
        LFun: TFun;
    begin   
        LFun:= @Add;
        writeln(LFun(1,2));
    end;

    procedure Cal(AFun: TFun);
    begin
        writeln(AFun(1, 2));
    end;

    begin
        CalAdd();
        Cal(@Add);
    end.

*类函数的使用*

    type
        TObjFun = function(X, Y: Real): Real of object;

    TMathOpr = class(TObject)
    public
        function Add(X, Y: Real): Real;
        function Sub(X, Y: Real): Real;
        function Multi(X, Y: Real): Real;
        function Division(X, Y: Real): Real;
        function Opr(AFun: TObjFun; X, Y: Real): Real;
    end;

    { TMathOpr }

    function TMathOpr.Add(X, Y: Real): Real;
    begin
        Result := X + Y;
    end;

    function TMathOpr.Division(X, Y: Real): Real;
    begin
        Result := X / Y;
    end;

    function TMathOpr.Multi(X, Y: Real): Real;
    begin
        Result := X * Y;
    end;

    function TMathOpr.Sub(X, Y: Real): Real;
    begin
        Result := X - Y;
    end;

    function TMathOpr.Opr(AFun: TObjFun; X, Y: Real): Real;
    begin
        Result := AFun(X, Y);
    end;

    var
        LMath: TMathOpr;
    begin
        LMath:= TMathOpr.Create;
        try
            with LMath do
            begin
                writeln(Opr(Add, 1,2));
                writeln(Opr(Sub, 1,2));
                writeln(Opr(Multi, 1,2));
                writeln(Opr(Division, 1,2));
            end;
        finally
            LMath.Free;
        end;
    end.

*匿名函数*

    type
        TFuncOfIntToString = reference to function(x: Integer): string;

        procedure AnalyzeFunction(proc: TFuncOfIntToString);
        begin
            { some code }
        end;

    function MakeAdder(y: Integer): TFuncOfIntToString;
    begin
        Result := 
            function(x: Integer): string
            begin
                Result := IntToStr(x+y);
            end;
    end;


    var
        myFunc: TFuncOfIntToString;
    begin
        myFunc:= MakeAdder(2);
        AnalyzeFunction(myFunc);

    
        AnalyzeFunction(
                function(x: Integer): string
                begin
                    Result := IntToStr(x);
                end;
        );
    end;

---

<h3 id="class" />

[类和对象](#context)

<h3 id="routine" />

### 标准函数和输入输出函数

<h3 id="library" /> 

### 库和包

<h3 id="interface" />

### 对象接口

<h3 id="memory" />

### 内存管理

<h3 id="control" />

### 程序控制

<h3 id="assembly" />

### 内联汇编代码（只针对windows）

<h3 id="generic" />

### 泛型  

<h3 id="attribute" />

### 属性（Attributes）运行时类型信息RTTI