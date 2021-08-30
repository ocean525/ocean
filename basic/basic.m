% 注释
% 1 定义整数，实数，字符串，数组，矩阵
a=int8(1.2)     %a=1 占用1个字节  uint64(-1)=0 占用8个字节 无符号
a=1.2   %whos a----double   single(a)=single
a='zifu'   %a直接覆盖原来的a，whos a---char-单引号-字符数组   a="zifu"---str--字符串数组
           %num2str(43)得到的将是'43',char(43)得到的将是'+',str2num可以直接用于char
a(10)=1 % 0     0     0     0     0     0     0     0     0     1  double; a(10)=int8(1)  int8
a(2,2)=0% a=zeros()

% 2 字符串拼接，切割，矩阵拼接，切割
a='a'
b='bc'
[a,b]   ---abc---1*3---ans(2)~'b'
clear a b
a(2,2)=0
b(1)=1
a(1,1)=b

% 3 特别类型
a{1}=1;a{2}='1'  %cell
% 4 循环
for index = 1:n
    body
end
% 5 判断
# 与或非


if condition
elseif condition
    body
else
    body
end

switch variable
    case 'string'
        body
    case 'string'
        body
    otherwise
        body
end
     
% 6 函数

function output = myFun(input)  % output=[a b c],input=[d e f]
%myFun - Description
%
% Syntax: output = myFun(input)
%
% Long description
    
end

[a1,b1,c1]=myFun(d1,e1,f1)

% 基本运算
1+1   %2
a=1
b=1
c=a+b

%{

若干语句

%}