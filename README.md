# LiutCL - A Common Lisp Implementation from Scratch

## 简介

一个简单的、针对个人Lisp方言的解释器。

## 实现的功能

* 2012年11月1日
  * 定义了结构体类型struct stream_t，用于实现Lisp级别的流对象，如文件流和字符流、字节流。
  * 在Lisp代码级别，实现了用于从文件流中读取输入的lt/read-a-char函数。
  * 在stream.c文件中增加了很多向文件流对象中写入数据的函数，如写入单个字符的write\_stream\_char、写入字符串的write\_stream\_string等。
* 2012年10月31日
  * 新定义结构体类型struct block_environment，对应的指针类型别名为BlockEnvironment，用于支撑lt/block所创建的词法作用域。其中成员变量name即为lt/block所使用的第一个参数，context成员变量则是用于longjmp进行跳转。lt/block可嵌套，所以需要一个prev指针链接外部的lt/block所创建的块。
  * 给大部分的eval\_\*函数增加一个BlockEnvironment类型的block_env参数，用于支撑lt/block和lt/return-from的实现。
  * 增加了和Common Lisp的两个特殊操作符相仿的功能
    * 和block类似的lt/block；
    * 和return-from类似的lt/return-from，这两个都是特殊操作符。
  * 在types.h文件中新增struct stream_t结构体类型
* 2012年10月17日
  * 把SymbolTable的实现从普通的二叉查找树更换为AVL树
* 2012年10月15日
  * 增加了一个通过标准输入和用户交互的功能
    * read_sexp函数，通过传递参数stdin就可以读取来自标准输入的输入。
  * 实现了CL中的catch和throw功能
    * 用符号lt/catch代替了catch
    * 用符号lt/throw代替了throw。这两个名字以后可能会修改为catch和throw。
    * 利用了C语言的setjmp和longjmp来实现以上的两个special operators。
* 2012年10月11日
  * 修改了一些lisp代码级别的函数名：
	* plus-two修改为add-two
	* mult-two修改为mul-two，命名采自汇编语言。
  * 修改了几个C代码级别的函数名：
    * plus\_two改为add\_two；
    * mult\_two改为mul\_two，与lisp代码级别的函数名保持一致；
    * new\_apply\_env改为new\_binding\_env
  * 将变量名和函数名分开为两个命名空间，即语言为Lisp-2。
  * 实现了特殊操作符lt/dset!，用于为符号在动态作用域中绑定值，如用于实现函数名和函数对象的绑定。
	* 其实，我更倾向于将这个功能实现在set!中，就像Common Lisp中的setq可以设定(symbol-function foo)这样的对象所对应的值一样。
  * 特殊操作符lt/dynamic，用于取出变量在动态环境中对应的值。
* 2012年10年09日
  * Scheme所使用的特殊操作符，包括quote、if、begin、set!和lambda。
  * 一些基本的函数
	* plus-two。实现两个整数的相加。
	* mult-two。实现两个整数的相乘。
	* quit。退出解释器。
	* gt-two。比较两个整数的大小关系。
	* and-two。逻辑与运算。
	* sub-two。实现两个整数的相减。
	* div-two。实现两个整数的相除。
	* or-two。逻辑或运算。
	* get-cons-car。仅作用于点对类型的对象，取其car成员。
	* get-cons-cdr。仅作用于点对类型的对象，取其cdr成员。
	* numeric-eq。判断两个整数是否相等。
	* lt-eq。判断任意两个对象是否*同一*。

## 实现细节

* 对于要预定义的特殊操作符的名字，即对应的符号，应该像下面这样操作
  * 在atom\_proc.c文件中定义全局变量，名字为lt\_操作符名
  * 在init\_symbol\_table函数中给上述定义的符号赋值
  * 在atom\_proc.h头文件中声明这个全局变量
* 目前函数名和对应的值，即函数体的绑定位于动态作用域中，在接下来我觉得应该把函数作为一个单独的环境实现，从动态作用域中剥离出来。
* 我希望给解释器增加一个底层的堆栈式的虚拟机，以方便直接地对函数调用的栈和帧进行管理，或许可以方便地实现多重返回值。
* 如果不是非常迫切的需要，我大概是不会使用指针中的位来表示一个对象的类型的。我觉得这样做牺牲了代码的可读性和可维护性。
  * 或许是时候考虑实现用位来表示数据对象的类型了→_→
