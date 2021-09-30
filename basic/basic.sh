# 注释
# 1 定义整数，实数，字符串，数组，矩阵
your_name="qinjx" #readonly your_name
num='2'             #int($num)
echo $your_name
echo ${your_name}
unset variable_name
#字符串是shell编程中最常用最有用的数据类型（除了数字和字符串，也没啥其它类型好用了），字符串可以用单引号，也可以用双引号，也可以不用引号。
#双引号的优点：双引号里可以--有变量--;双引号里可以出现--转义字符--

# 数组
array_name=(value0 value1 value2 value3)
array_name[1]=value1
echo ${数组名[下标]}
echo ${array_name[@]}  #使用 @ 符号可以获取数组中的所有元素
# 取得数组元素的个数
length=${#array_name[@]}
# 或者
length=${#array_name[*]}
# 取得数组单个元素的长度
lengthn=${#array_name[n]}


# 2 字符串拼接，切割，矩阵拼接，切割
your_name="runoob"
# 使用双引号拼接
greeting="hello, "$your_name""    hello, runoob
greeting_1="hello, ${your_name}"  hello, runoob--有变量--
echo $greeting  $greeting_1
# 使用单引号拼接
greeting_2='hello, '$your_name''  hello, runoob
greeting_3='hello, ${your_name}' ----hello, ${your_name}--有变量--显示不出来
echo $greeting_2  $greeting_3
# 获取字符串长度
string="abcd"
echo ${#string} #输出 4
# 截取子字符串
string="runoob is a great site"
echo ${string:1:4} # 输出 unoo

# 3 特别类型
| 管道命令，$0是前一个命令传输过来的

# 4 循环
for var in item1 item2 ... itemN; do command1; command2… done;
#  in 列表可以包含替换、字符串和文件名
for var in item1 item2 ... itemN
do
    command1
    command2
    ...
    commandN
done
#
for str in This is a string
do
    echo $str
done

#
while condition
do
    command
done
#
int=1
while(( $int<=5 ))
do
    echo $int
    let "int++"
done
#
无限循环
无限循环语法格式：

while :
do
    command
done
或者

while true
do
    command
done
或者

for (( ; ; ))
# break and continue

# 5 判断
if [ $(ps -ef | grep -c "ssh") -gt 1 ]; then echo "true"; fi
#
a=10
b=20
if [ $a == $b ]          [后面的空格是必须的
then
   echo "a 等于 b"
elif [ $a -gt $b ]
then
   echo "a 大于 b"
elif [ $a -lt $b ]
then
   echo "a 小于 b"
else
   echo "没有符合的条件"
fi

#
if test $[num1] -eq $[num2]
then
    echo '两个数字相等!'
else
    echo '两个数字不相等!'
fi
#
echo '输入 1 到 4 之间的数字:'
echo '你输入的数字为:'
read aNum
case $aNum in
    1)  echo '你选择了 1'
    ;;
    2)  echo '你选择了 2'
    ;;
    3)  echo '你选择了 3'
    ;;
    4)  echo '你选择了 4'
    ;;
    *)  echo '你没有输入 1 到 4 之间的数字'
    ;;
esac

# 6 函数
demoFun(){
    echo "这是我的第一个 shell 函数!"
}

demoFun #调用

#
funWithReturn(){
    echo "这个函数会对输入的两个数字进行相加运算..."
    echo "输入第一个数字: "
    read aNum
    echo "输入第二个数字: "
    read anotherNum
    echo "两个数字分别为 $aNum 和 $anotherNum !"
    return $(($aNum+$anotherNum))
}
funWithReturn
echo "输入的两个数字之和为 $? !"
函数返回值在调用该函数后通过 $? 来获得，连续使用两次 echo $?，得到的结果不同，更为直观第一次是返回值，第二次就是0
$?	显示最后命令的退出状态。0表示没有错误，其他任何值表明有错误。
#
funWithParam(){
    echo "第一个参数为 $1 !"
    echo "第二个参数为 $2 !"
    echo "第十个参数为 $10 !"
    echo "第十个参数为 ${10} !"
    echo "第十一个参数为 ${11} !"
    echo "参数总数有 $# 个!"
    echo "作为一个字符串输出所有参数 $* !"
}
funWithParam 1 2 3 4 5 6 7 8 9 34 73
在函数体内部，通过 $n 的形式来获取参数的值,$1表示第一个参数，n>=10时，需要使用${n}来获取参数
$#	传递到脚本或函数的参数个数


# 7 基本运算
a=$[2*3]
echo $a # 6


:<<EOF
其实冒号在Bash里也是一个命令，表示啥都不做，<<是输入重定向，
两个EOF(可用其它特殊成对字符替代)之间的内容通过<<追加给冒号（:），
但是冒号对它们啥都不做，就相当于没做任何处理和输出，就相当于注释了
EOF

