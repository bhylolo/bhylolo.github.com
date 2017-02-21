Learing Visual Studio Code
==

# VS Code编辑器快捷键

|Function|Shortcuts|Remarks|
|--|--|--|
|显示所有命令|Ctrl+Shift+P|可能会有其他程序的快捷键与此冲突，例如截屏软件
|转到文件|Ctrl+P|
|在文件中查找|Ctrl+Shift+F|
|开始调试|F5|
|切换终端|Ctrl+`|
|

# [集成终端]
## 打开终端的方式
>* 快捷键 **`Ctrl+` `**
>* 菜单栏 **`View|Integrated Terminal `**
>* 从**Command Palette `Ctrl+Shift+P`**，使用 **`View:Toogle Integrated Terminal`**

[集成终端]: https://code.visualstudio.com/docs/editor/integrated-terminal

## 多终端管理
添加终端的快捷键 **`Ctrl+Shift+` `**

# [Debugging](https://code.visualstudio.com/docs/editor/debugging)
VS Code一个关键功能就是对于debugging的支持，VS Code内嵌的调试器debugger能够让你高效的进行编码，编译和调试的过程。

![调试器界面](https://code.visualstudio.com/images/debugging_debugging_hero.png "调试器界面")

VS Code包含一个内嵌的debugger，它支持Node.js运行时，且可以调试JavaScript、TypeScript、以及其他可以编译成Javascript的语言。

对于调试其他语言和运行时（包括PHP、Ruby、Go、C#、Python等）请在[市场](https://marketplace.visualstudio.com/search?target=vscode&category=Debuggers&sortBy=Downloads)中寻找[debugger扩展](https://code.visualstudio.com/docs/editor/extension-gallery)。

>*注：以下的介绍是基于内嵌的Node.js调试器（debugger），但是大部分的概念和功能同样适用于其他语言的debugger。*

>*如果是对于Node.js不是非常熟悉，在debugging之前，遵循[Node.js 演示](\blog\Learning_JavaScript)安装Node.js并创建一个`hello word`的程序（`app.js`）。一旦你已经拥有一个简单的js程序，以下的步骤想带你了解 `VS Code` debugging的功能*


在VS Code边栏上点击debugging的图标就可以展开debug窗口
![debug view](https://code.visualstudio.com/images/debugging_debugicon.png) 

debug窗口中包含了所有有关debugging的信息，它在debugging命令和配置设置上还有一个工具栏。

如果只是简单的debug一个程序，在VS Code中只需要按 **`F5`**

# [Transact-SQL]
## 安装T-SQL支持
通过安装mssql扩展包来使VS Code支持T-SQL，通过VS Code应用市场安装步骤如下：
>1. 从VS Code边栏打开扩展窗口 **`Ctrl+Shift+X`**
>2. 在搜索框中输入**mssql**，点击**install** 然后重新加载VS Code

## 连接和执行T-SQL
1. 在第一次安装完`mssql`扩展之后，打开一个`sql`文件，`VS Code`会自动初始化`mssql`扩展包，并自动从网上下载安装`SQL Tools Service`，解压安装
2. 执行`sql`文件:
    * `Ctrl+Shift+C`(`Ctrl+Shift+p -> ms sql: connect`)
    (*如果是第一次创建连接，会提示创建一个profile，否则，选择一个已有的profile即可。*)    
    * `Ctrl+Shift+E `执行SQL文件，
    (*如果没有制定连接，或者没有连接配置文件，会在执行前提示制定连接配置*)
3. 执行的结果会在一个新的视窗（view）中以文本的形式显示。

[Transact-SQL]: https://code.visualstudio.com/docs/languages/tsql