# LiutCL - 挖坑必填

## 简介

一个简单的、针对个人Lisp方言的解释器。

## 实现的功能

* Tue Oct  9 22:33:16 2012
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
