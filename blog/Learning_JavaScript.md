**Learning JavaScript**
==
>[语法](#grammar)
>
>[Node.js](#nodejs)


* * *
<h1 id="grammar">语法</h1>
==
## 基本类型
|类型|示例|说明
|--|--|--|
|undefined|v=undefined|未声明的变量，或者声明过但未赋值的变量
|string| v='...' <br> v="..."  |不能直接读取或修改字符串中的单一字符<br/>字符串在赋值运算中，按照引用类型的方式来处理
|number|v=123|
|boolean|v=true <br> v=false|
|object|v=null <br>v={...}<br>|
|function|v= function() {...}|函数包含：函数、方法、构造器、类、函数对象等
|

## 运算符
|运算符|名称|说明|
|--|--|---|
|=|赋值||
|===|全等|用于`对值类型、引用类型`的实际数据进行比较（值，也包括undefined）<br/>对于`引用类型`全等意味着不但`值`相等，引用也指向同一个
|==|等于|仅仅只比较`值`知否相等，对于引用类型，忽略是否指向同一引用

## 转义字符
|符号|说明|符号|说明
|--|--|--|--|
|\b|退格符|\\'|单引号
|\t|水平制表符|\\"|双引号|
|\v|垂直制表符|\\\ |反斜线 
|\n|换行符|\0|null字符串，它不是作为字符串的结束符，可以出现在字符串中
|\r|回车符|\xnn|ASC码为nn的字符转译成unicode
|\f|换页符|\unnn|unicode字符码为nnn的字符
||||

## 运算符
|符号|说明|示例|
|--|--|--|
|typeof|获取变量或值的类型||
|void|运算表达式并忽略值||
|new|创建指定类的对象实例||
|in|检查对象属性||
|instanceof|检查变量是否指定类的实例||
|delete|删除实例属性||
|+-*/%|数值运算，number||
||||
||||
||||
||||
||||
||||
||||

<h1 id="nodejs"></h1>

# [Node.js](https://code.visualstudio.com/docs/runtimes/nodejs)
[Node.js](https://nodejs.org/en/)是一个平台，它让JavaScript的程序更快、更有扩展性的运行在服务器端。Node.js是一个运行时环境(`runtime`)，它使用[NPM](https://www.npmjs.com/)来管理Node.js的模块。