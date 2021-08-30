# -* - coding: UTF-8 -* -
#! /usr/bin/python 用于指定解释器
# 注释  所有代码块语句必须包含相同的缩进空白数量
# 因此，在 Python 的代码块中必须使用相同数目的行首缩进空格数。
# 建议你在每个缩进层次使用 单个制表符 或 两个空格 或 四个空格 , 切记不能混用
# 1 定义整数，实数，字符串，数组，矩阵
counter = 100 # 赋值整型变量 a = b = c = 1
miles = 1000.0 # 浮点型  a, b, c = 1, 2, "john"
name = "John" # 字符串
del counter,name
print,counter
print(name)
#Python有五个标准的数据类型：Numbers（数字）String（字符串）List（列表）Tuple（元组）Dictionary（字典）
#Python支持四种不同的数字类型：int（有符号整型）long（长整型，也可以代表八进制和十六进制）float（浮点型）complex（复数）


# 2 字符串拼接，切割，矩阵拼接，切割
s = 'abcdef'
a = s[1:5]
'bcde'

print, s + s    # 打印组合的列表

a=a[:6] + 'Runoob!'
#[1,2,3] 是 List 类型，"Runoob" 是 String 类型，而变量 a 是没有类型，她仅仅是一个对象的引用（一个指针），可以是 List 类型对象，也可以指向 String 类型对象



# 3 特别类型
#List（列表） 是 Python 中使用最频繁的数据类型。
#列表可以完成大多数集合类的数据结构实现。它支持字符，数字，字符串甚至可以包含列表（即嵌套）
list = [ 'runoob', 786 , 2.23, 'john', 70.2 ]
#元组tuple 用 () 标识。内部元素用逗号隔开。但是元组不能二次赋值，相当于只读列表
tuple = ( 'runoob', 786 , 2.23, 'john', 70.2 )
#字典(dictionary),字典用"{ }"标识,是除列表以外python之中最灵活的内置数据结构类型。列表是有序的对象集合，字典是无序的对象集合
#字典由索引(key)和它对应的值value组成
dict = {}
dict['one'] = "This is one"
# 4 循环        break continue
'''
    while(表达式) :
   …
    else :
   …

    for 变量 in 集合 :
   …
    else :
   …

    for x in range(0,5,2):
    print, x
# 5 判断
逻辑表达式中and表示逻辑与，or表示逻辑或，not表示逻辑非
in	如果在指定的序列中找到值返回 True，否则返回 False。	x 在 y 序列中 , 如果 x 在 y 序列中返回 True。
not in	如果在指定的序列中没有找到值返回 True，否则返回 False。	x 不在 y 序列中 , 如果 x 不在 y 序列中返回 True。
包括字符串，列表或元组

 id() 函数用于获取对象内存地址
 is	is 是判断两个标识符是不是引用自一个对象	x is y, 类似 id(x) == id(y) , 如果引用的是同一个对象则返回 True，否则返回 False
 is not	is not 是判断两个标识符是不是引用自不同对象	x is not y ， 类似 id(a) != id(b)。如果引用的不是同一个对象则返回结果 True，否则返回 False。

if (表达式) :
  语句1
elif (表达式) :
  语句2
…
elif (表达式) :
  语句n
else :
  语句m

'''

# 6 函数
def functionname( parameters ):
        #  "函数_文档字符串"
        #function_suite
        #  return [expression]


#functionname( parameters )

# 基本运算
    a = 21
    b = 10
    c = 0
 
    c = a + b

'''
这是多行注释，使用单引号。
这是多行注释，使用单引号。
这是多行注释，使用单引号。
'''



''' 格式转换
int(x [,base])

将x转换为一个整数

long(x [,base] )

将x转换为一个长整数

float(x)

将x转换到一个浮点数

complex(real [,imag])

创建一个复数

str(x)

将对象 x 转换为字符串

repr(x)

将对象 x 转换为表达式字符串

eval(str)

用来计算在字符串中的有效Python表达式,并返回一个对象

tuple(s)

将序列 s 转换为一个元组

list(s)

将序列 s 转换为一个列表

set(s)

转换为可变集合

dict(d)

创建一个字典。d 必须是一个序列 (key,value)元组。

frozenset(s)

转换为不可变集合

chr(x)

将一个整数转换为一个字符

unichr(x)

将一个整数转换为Unicode字符

ord(x)

将一个字符转换为它的整数值

hex(x)

将一个整数转换为一个十六进制字符串

oct(x)

将一个整数转换为一个八进制字符串
'''