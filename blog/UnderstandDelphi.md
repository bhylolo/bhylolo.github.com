# 了解Delphi的特性

## 一些语法特性
### 结构体
#### 如何构造record
##### 自动构造/析构
##### New/Dispose
#### 结构体一定比对象效率高吗？

---
### Public与Published有什么不同
---
### 匿名函数
#### 作用域
#### 实现Iterator模式
---
### 从TObject，TPersistent，TComponent，TCollection/TCollectionItem 继承有什么不同
---
### 如何使用Interface
#### Interface的作用
#### 使用Interface实现注册中心
在框架中有一种需求是，希望实现一个注册中心来解绑框架与各个package之间的依赖。
#### 如何看待使用引用计数来自动管理对象的生命周期
#### IInterface 和 IDispatch
---
## VCL的一些特性
### 对象的序列化
#### 可序列化的对象和属性
#### 定制序列化的内容
#### 常用的序列化相关函数
---
### TClientDataSet的一些使用技巧
#### 使用数据感知控件
#### 创建结构
#### 增删改查
#### 遍历
当DataSet和数据感知UI控件绑定时，为了提高遍历或查询性能，需要在解除/重新绑定游标滚动时间 
#### ADT/Array Field
---
## Delphi的多线程编程
### Thread对象
#### synchronized 方法
---
## IDE 使用技巧
### 建立UML工程
---
### 快捷键
|快捷键|功能|
|--|--|
|Ctrl+D|格式化代码|
|Ctrl+J|代码模板|
|Ctrl+Y|删除当前行|
|Ctrl+Shift+U/I|往左/右缩进一个Tab|
|Ctrl+F12|打开project中的unit|
|Shift+F2|打开project中的Form|
|Alt+F11|添加工程中的unit到当前单元implements 下的uses|
|Ctrl+F11|将文件添加到当前project中|
|Shift+F11|打开文件（不一定是Project中的unit）|
|||

---
### 代码模板
#### 快捷键
<快捷代码>+Ctrl+J

#### 如何创建代码模板
---