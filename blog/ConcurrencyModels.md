# 如何使用Delphi创建多线程程序

> 以下内容摘自Delphi XE2 Help: HOW TO BUILD MULTITHREADED APPLICATION

- [构建一个多线程程序](#Building)
- [撰写cleanup code](#Writing)
- [避免多个线程同时访问同一块内存](#Avoiding)
- [声明一个线程对象](#Defining)
- [捕获异常](#Handling)
- [初始化线程](#Initializing)
- [使用VCL主线程](#Using)
- [线程等待](#Waiting)
- [撰写线程方法](#ThreadFunction)

---
<h2 id="Building">构建一个多线程程序</h2>
以下是在VCL窗体程序中，创建一个多线程程序所需的必要步骤：
1. 创建一个VCL Form，中间包含声明一个线程对象
2. 初始化线程（可选）
3. 撰写线程方法
4. 撰写线程Cleanup Code（可选）

---
<h2 id="Writing">撰写Cleanup Code</h2>
当线程对象结束时会执行cleanup代码
1. 确保处理cleanup逻辑的代码是撰写（挂接）在OnTerminate的事件中
2. 不要在cleanup code中引用任何线程对象中的变量，因为cleanup code是在线程结束时调用的，线程对象的局部变量此时访问会出错
3. 你可以在OnTerminate事件方法中安全的访问任何（主线程）的对象。

---
<h2 id="Avoiding">避免多个线程同时访问同一块内存</h2>
使用以下的技术可以有效的避免多个线程对于同一块内存在同一时间的访问

**Lock Object**
1. 对于一些对象，例如Canvas，拥有Lock方法，在必要时执行Lock方法可以避免其它对象对于此对象的访问，当不需要是，执行Unlock方法解除阻止
2. TThreadList.LockList (Delphi)可以阻塞其它线程对于它的访问，然后再调用TThreadList.UnlockList解除阻塞。

> Note: TCanvas.Lock and TThreadList.LockList 是线程安全的。

**使用临界区**

1. 创建一个全局对象TCriticalSection.
2. 调用方法Acquire阻止线程内对于全局内存的访问.
3. 调用方法Release之后，其它线程就可以继续访问全局内存。以下的代码中，包含一个全局临界区变量LockXY用以阻止对于全局变量X，Y的访问.如果要使用X，Y全局变量，则线程需要在临界区程序块中访问，代码如下：

        LockXY.Acquire;
        try
          X := X + 1;
          Y := sin(X);
        finally
          LockXY.Release
        end;

> 注意: 必须所有的访问线程都使用临界区访问全局内存，才能正常工作，否则就会遇到竞争访问的问题。

**使用“多读-排他写”的同步机制**

1. 创建全局对象TMultiReadExclusiveWriteSynchronizer，它将关联你将需要保护的全局内存
2. 在任何线程读访问之前，需要调用BeginRead方法。
3. 在读访问完毕后，需要调用EndRead方法。
4. 在任何线程写访问之前，需要调用BeginWrite方法。
5. 在写访问结束之后，线程需要调用EndWrite方法。

> 注意: 该同步机制要求所有访问的线程都使用该机制，否则就会出现访问竞争的问题。

---
<h2 id="Defining">声明一个线程对象</h2>

**如何声明一个thread对象**

1. 选择 **File > New > Other > Delphi Projects > Delphi Files**， 双击**Thread Object**。弹出对话框**The New Thread Object**。
2. 输入class name，例如 *TMyThread*
3. 输入（可选）输入thread的name，例如 MyThreadName.

      > 说明: 输入name虽然对于thread不是必须的，但是有意义的标识，有利于在调试线程。

4. Click OK.

以下是thread代码示例：

    unit Unit1;

    interface

    uses
      Classes;

    type
      TMyThread = class(TThread)
      private
        { Private declarations }
      protected
        procedure Execute; override;
      end;

    implementation

    { Important: Methods and properties of objects in visual components can only be
      used in a method called using Synchronize, for example,

          Synchronize(UpdateCaption);

      and UpdateCaption could look like,

        procedure TMyThread.UpdateCaption;
        begin
          Form1.Caption := 'Updated in a thread';
        end; }

    { TMyThread }

    procedure TMyThread.Execute;
    begin
      { Place thread code here }
    end;

    end.

Adding a name for the thread adds a call of the TThread class method NameThreadForDebugging to the TThread Execute procedure:

    unit Unit1;

    interface

    uses
      Classes;

    type
      TMyThread = class(TThread)
      private
        { Private declarations }
      protected
        procedure Execute; override;
      end;

    implementation

    { Important: Methods and properties of objects in visual components can only be
      used in a method called using Synchronize, for example,

          Synchronize(UpdateCaption);

      and UpdateCaption could look like,

        procedure TMyThread.UpdateCaption;
        begin
          Form1.Caption := 'Updated in a thread';
        end; }

    { TMyThread }

    procedure TMyThread.Execute;
    begin
      NameThreadForDebugging('My Cool Thread');
      { Place thread code here }
    end;

    end.

---
<h2 id="Handling">捕获异常</h2>

在Execute方法中添加try...except代码块来捕获异常。

      procedure TMyThread.Execute;
      begin
        try
          while not Terminated do
            PerformSomeTask;
        except
          {do something with exceptions}
        end;
      end;

---
<h2 id="Initializing">初始化线程</h2>

**初始化线程**

1. 指定线程的默认优先级
2. 确定线程的退出条件

**为线程指定优先级**

1. 以下列出了线程优先级的参数，一般原则上使用高优先级的的线程来处理时间关键性的任务，使用低优先级的线程处理其它任务。

|值|优先级|
|--|--|
|tpIdle |Windows不会中断其它线程来执行tpIdle级别的线程，只有当系统空闲时，该级别的线程才会被执行
|tpLowest |比normal低两级
|tpLower |比normal低一级
|tpNormal |normal级
|tpHigher |比normal高一级
|tpHighest |比normal高两级
|tpTimeCritical |最高优先级
|||


2. Override the Create method (Delphi)添加新的构造函数逻辑
3. 以下示例展示了thread构造函数的写法（低优先级线程）：

        constructor TMyThread.Create(CreateSuspended: Boolean);
        begin
          inherited Create(CreateSuspended);
          Priority := tpIdle;
        end;

4. 确定在线程结束时，是否需要自动释放thread对象。

> 注意：对于CPU密集型计算的线程，提高其优先级会导致其它线程处于“饥饿状态”，只有对于那些主要时间是用来等待外部事件的线程，可以提高其优先级。

**指定线程释放条件**

1. 设置FreeOnTerminate属性为true（除非该线程是由其它线程负责管理的）
2. 如果需要由另外一个线程负责管理，则将此线程FreeOnTerminate属性设置为false，则此线程的释放由另外的线程负责。

---
<h2 id="Using">使用VCL主线程</h2>
使用VCL主线程遵循以下步骤:

1. 声明一个单独的函数用于接收程序中控件的Windows消息。
2. 周期性执行函数CheckSynchronize
3. 声明线程局部变量（thread-local variables）,当你的线程需要排他访问时，这种方式是必须的。

**声明一个单独的函数用于接收Windows消息**

1. 在主线程中声明一个函数，用以访问主线程中对象方法、属性。
2. 在线程execute的方法中执行TThread.Synchronize方法，用以同步调用主线程的事件:

    procedure TMyThread.PushTheButton
    begin
      Button1.Click;
    end;
    procedure TMyThread.Execute;
    begin
      // ...
      Synchronize(PushThebutton);
      // ...
    end;

**Synchronize**方法会等待主线程，进入主线程消息循环执行外部方法。

>注意：由于Synchronize使用了消息循环, 它不能工作在控制台程序(console applications)。对于控制台程序，使用其它机制，例如临界区（critical sections），来保护对于VCL对象的访问。

**To call CheckSynchronize**

1. 在主线程中周期性的调用CheckSynchronize方法，用来主线程同背后的执行线程间做同步。
2. 在OnIdle事件中，当程序空闲（idle）时，调用CheckSynchronize方法确认背后执行线程执行状态。

**使用线程局部变量**

当你需要声明一个变量，它既能被全局函数访问，但不想被同一个thread的其它实例（Instance）访问的话，使用**threadvar**声明变量：

    threadvar
      x: integer;

> 注意：threadvar只能针对全局变量声明，不能用于声明指针和函数变量、以及使用copy-on-write机制的类型（例如 long strings）。

---
<h2 id="Waiting">线程等待</h2>
以下方法被用来等待线程

- 等待线程executing结束
- 等待task完成
- 检查是否有另外的线程正在等待当前线程的结束状态

**等待一个线程executing执行结束**

1. 使用线程的WaitFor方法
2. 撰写代码逻辑，例如，以下示例是在访问list中对象之前，等待另一个线程填充thread list:

        if ListFillingThread.WaitFor then
        begin
          with ThreadList1.LockList do
          begin
            for I := 0 to Count - 1 do
              ProcessItem(Items[I];
          end;
          ThreadList1.UnlockList;
        end;

**等待任务的结束**

1. 创建一个全局的TEvent对象
2. 如果其它线程正在等待一个线程的处理结束，则在结束时，该处理线程调用TEvent.SetEvent方法
3. 调用TEvent.ResetEvent方法，关闭信号开关

以下代码示例是在OnTerminate事件里,在临界区中，使用一个全局计数器线程线程用来追踪线程数量，当线程数量减少到0时，调用SetEvent方法去通知：所有线程已经都释放了：

    procedure TDataModule.TaskTerminateThread(Sender: TObject);
    begin
      ...
      CounterGuard.Acquire; {obtain a lock on the counter}
      Dec(Counter); {decrement the global counter variable}
      if Counter = 0 then
        Event1.SetEvent; {signal if this is the last thread}
      Counter.Release; {release the lock on the counter}
      ...
    end;

主线程初始化计数器*Counter*, 启动任务线程，等待所有线程执行执行完毕后调用**TEvent.WaitFor**修改状态量。**WaitFor**方法等待指定时间后返回信号量，信号量值见下表。

以下代码展示了如何在主线程中启动一个任务线程，以及任务执行完成后如何恢复线程:

    Event1.ResetEvent; {clear the event before launching the threads}
    for i := 1 to Counter do
      TaskThread.Create(False); {create and launch the task threads}
    if Event1.WaitFor(20000) <> wrSignaled then
      raise Exception;
    {continue with main thread}

>注意：如果你不希望在一段时间后终止对于线程的等待，可以在WaitFor的方法参数上指定INFINITE。对于INFINITE的使用需要非常小心，因为如果预期的信号量没有到达的话，你的线程很有可能会被挂起。

**检查是否有另外的线程正在等待当前线程的结束状态**

1. 在当前线程中,实现Terminate检查或响应Terminated属性
2. 在Execute方法中的一种实现方式，如下所示：

        procedure TMyThread.Execute;
        begin
          while not Terminated do
            PerformSomeTask;
        end;

**WaitFor返回值**

|Value|Meaning|
|--|--|
|wrSignaled|The signal of the event was set.
|wrTimeout|The specified time elapsed without the signal being set.
|wrAbandoned |The event object was destroyed before the timeout period elapsed.
|wrError |An error occurred while waiting.


---
<h2 id="ThreadFunction">撰写线程方法</h2>

Execute是线程方法，你可以将它认为是你主程序启动的一个程序，所不同的，线程会共享你当前的进程空间。撰写线程方法会比写一个程序（program）稍微复杂一点，因为你必须关心线程不会复写（overwrite）主程序的共享内存区域。另一方面，由于线程会在主程序中同其它线程共享内存，你可以使用共享内存来传递信息。

# Seven Concurrency Modules in Seven Weeks *[Am] Paul Buther*

- [概述](#summary)
- [线程和锁](#ThreadLock)
- [函数式编程](#FunctionalProgram)
- [分离标志与状态](#isolation)
- [Actor](#Actor)
- [通讯顺序进程](#communication)
- [数据并行](#DataParallel)
- [Lambda](#Lambda)


<h2 id="summary">概述</h2>

### 并发还是并行
- **并发** 是问题域中的概念,是指程序被设计成可以处理多个几乎同时发生的事件.
- **并行** 是方法域中的概念,通过将问题中的多个部分同时执行,来加速问题的解决.

### 并行架构
- bit-level 位级并行
- instruction-leve 指令级并行

<h2 id="ThreadLock">线程和锁</h2>
<h2 id="FunctionalProgram">函数式编程</h2>
<h2 id="isolation">分离标志与状态</h2>
<h2 id="Actor">Actor</h2>
<h2 id="communication">通讯顺序进程</h2>
<h2 id="DataParalle">数据并行</h2>
<h2 id="Lambda">Lambda</h2>
