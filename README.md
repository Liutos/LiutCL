# LiutCL - 挖坑必填

## 简介

一个简单的、针对个人Lisp方言的解释器。

## 实现的功能

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
