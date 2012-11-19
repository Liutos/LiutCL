# LiutCL - A Approximate Common Lisp Implementation from Scratch

## 简介

自认为是一个Common Lisp实现

## 实现的功能

* 2012年11月19日
  * 支持可选（&optional）参数和关键字（&key）参数的默认值的设置
* 2012年11月11日
  * 在光棍节的这一天，我居然可耻地实现多重返回值（multiple values return）QAQ。
* 2012年11月10日
  * 函数化特殊操作符
  * 用Frame而不是普通的列表传递函数参数
  * 对浮点数、关键字符号和qualified符号的支持。
* 2012年11月8日
  * tagbody和go特殊操作符
  * rplaca、rplacd、list等函数。
* 2012年11月5日
  * 将Lisp级别的特殊操作符lt/catch和lt/throw修改为catch和throw，将lt\_begin修改为lt\_progn。
  * 去掉了lt/dynamic的C和Lisp级别的定义
  * 名字修改：
    * add_two改为add、对应的add-two改为add，另外三个四则运算函数同样；
    * and-two和or-two分别改为and2和or2；
    * numeric_eq改为fixnum_eq，对应的numeric-eq改为fixnum-eq。
  * 增加参数个数检查功能check_arity
* 2012年11月3日
  * 以tagged pointer的方式定义了函数对象
  * 将原来出现INTEGER枚举值的地方全部修改为FIXNUM，并修改了相应的宏定义。
  * 将read\_stream\_char更名为read\_char；将write\_stream\_char更名为write\_char；将write\_stream\_string更名为write\_string。
  * 将liutcl.c中的局部变量dynamic\_env修改为名为global\_dynamic\_env
* 2012年11月2日
  * 修改了INTEGER宏的定义，将LispObject类型的对象视为tagged pointer处理，通过移位操作来得到真正的无符号整数的值。
  * 在primitive.c文件中定义了宏ACCESS\_PARM2，以方便在四则运算函数中的代码编写。
  * 将较小的无符号整数和*点对*类型用tag进行表示，减轻了指针解引用的次数。
  * 将符号和字符类型用tag进行表示，理由同上。
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

* 对于要预定义的特殊操作符的对应的符号，应该执行以下步骤：
  * 在atom\_proc.c文件中定义全局变量，命名约定为lt\_操作符名；
  * 在atom\_proc.h头文件中声明这个全局变量；
  * 在init\_special\_operators函数中初始化上述定义的符号。
* types.h中的命名约定：
  * 在types.h文件中，所有形如TO\_XXX的宏均为类型转换宏，主要用于将普通的数据转换为相应的tagged pointer形式存储的数据，它们分别对应于形如theXXX的宏，后者用于从LispObject中取出需要的值。
* 将cons\_t和symbol\_t类型从LispObject中独立出来，与将Fixnum和Character从LispObject的做法是不同的：
  * 对于Fixnum和Character而言，其存储需要使用字中的每一bit。为了以tagged pointer形式存储，需要先左移。
  * 对于Cons和Symbol，本质为指针，因此低位在对齐后有空闲的bit，可以直接用bitwise-or写入类型信息。
  * 所有以tagged pointer形式存储的数据，都不具备type成员变量。
* ensure\_symbol\_exists函数只负责生成名字与参数*相同*的符号，不负责进行名字的大小写转换，因此在源代码中调用这个函数的地方都要用全大写字符串作为参数。
* 关于条件预处理：
  * 在CFLAGS中定义的宏FS，意为Functional Special operators，当需要将特殊操作符定义为函数原语，而不是实现成eval\_*时所使用。
* 用alist来表示关键字参数和对应的参数值之间的对应关系

## Bugs

* 在lambda中无法使用完全的ordinary lambda list
  * 这是因为调用由lambda构造出来的函数等价于在扩展过的词法环境中去依次求值函数体的代码，而这个扩展的过程是由函数extend\_env\_by\_cons完成的。但是这个函数的功能很简单，无法识别出一个对象究竟是specifier还是parameter，所以它会把specifier当作参数名进行处理，这样会导致扩展出来的词法环境多了绑定，而且顺序也不对。
  * 既然单纯地把参数列表和值联系起来是错误的，那么就应该结合一些其它信息来决定将哪一个参数名和哪一个值联系起来。这样的信息显然应该是根据lambda-list得出来的，大概可以猜测到包含下面这些信息：
    1. 必要参数的数量；
    2. 可选参数的数量；
    3. 可选参数是否有初始值；
    3. 可选参数是否有对应的-p参数；
    4. 是否有&rest限定符；
    5. 关键字参数的数量；
    6. 关键字参数是否指定了*关键字名字*（当关键字参数被置于两重cons中时，第二重cons的car即为*关键字名字*）；
    7. 关键字参数是否指定了初始值；
    8. 关键字参数是否有对应的-p参数；
    9. 是否有&allow-other-keys限定符；
    10. 是否有aux参数；
    11. aux参数是否有初始值。
