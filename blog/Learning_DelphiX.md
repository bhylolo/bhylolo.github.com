# Learning Delphi X
## Delphi语法

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

### 语言概述
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

一个VCL 窗体文件中包含了所对应的窗体`form`或控件`component`的属性值。每一个窗体文件都对应一个窗体（可能是windows窗体或者是对话框）。通常，由于文本
A VCL form file contains the description of the properties of the form and the components it owns. Each form file represents a single form, which usually corresponds to a window or dialog box in an application. The IDE allows you to view and edit form files as text, and to save form files as either text (a format very suitable for version control) or binary. Although the default behavior is to save form files as text, they are usually not edited manually; it is more common to use Embarcadero's visual design tools for this purpose. Each project has at least one form, and each form has an associated unit (.pas) file that, by default, has the same name as the form file. 

In addition to VCL form files, each project uses a resource (.res) file to hold the application's icon and other resources such as strings. By default, this file has the same name as the project (.dpr) file. 

A project options (.dof) file contains compiler and linker settings, search path information, version information, and so forth. Each project has an associated project options file with the same name as the project (.dpr) file. Usually, the options in this file are set from Project Options dialog. 

Various tools in the IDE store data in files of other types. Desktop settings (.dsk) files contain information about the arrangement of windows and other configuration options; desktop settings can be project-specific or environment-wide. These files have no direct effect on compilation. 

**Compiler-Generated Files**

The first time you build an application or a package, the compiler produces a compiled unit file (.dcu on Win32) for each new unit used in your project; all the .dcu files in your project are then linked to create a single executable or shared package. The first time you build a package, the compiler produces a file for each new unit contained in the package, and then creates both a .dcp and a package file. If you use the GD compiler switch, the linker generates a map file and a .drc file; the .drc file, which contains string resources, can be compiled into a resource file. 

When you build a project, individual units are not recompiled unless their source (.pas) files have changed since the last compilation, their .dcu/.dpu files cannot be found, you explicitly tell the compiler to reprocess them, or the interface of the unit depends on another unit which has been changed. In fact, it is not necessary for a unit's source file to be present at all, as long as the compiler can find the compiled unit file and that unit has no dependencies on other units that have changed. 

#### Example Programs

The examples that follow illustrate basic features of Delphi programming. The examples show simple applications that would not normally be compiled from the IDE; you can compile them from the command line. 

**A Simple Console Application**

The program below is a simple console application that you can compile and run from the command prompt: 

    program Greeting;
    
    {$APPTYPE CONSOLE}
    
    var
      MyMessage: string;
    
    begin
      MyMessage := 'Hello world!';
      Writeln(MyMessage);
    end.

The first line declares a program called Greeting. The {$APPTYPE CONSOLE} directive tells the compiler that this is a console application, to be run from the command line. The next line declares a variable called MyMessage, which holds a string. (Delphi has genuine string data types.) The program then assigns the string "Hello world!" to the variable MyMessage, and sends the contents of MyMessage to the standard output using the Writeln procedure. (Writeln is defined implicitly in the System unit, which the compiler automatically includes in every application.) 

You can type this program into a file called `greeting.pas` or `greeting.dpr` and compile it by entering: 

`dcc32 greeting` 

to produce a Win32 executable. 

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

### 过程和函数

<h3 id="class" />

### 类和对象

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