+++
title = "python 作用域与闭包"
author = ["flumine@qq.com"]
date = 2022-12-28T23:40:00+08:00
lastmod = 2022-12-28T23:40:44+08:00
tags = ["python"]
categories = ["技术"]
draft = false
+++

## 作用域 {#作用域}

python 在运行时如何搜索变量？是通过作用域。首先在 local scope 查找，如果找到就直接用，没有找到就向包裹这个 local scope 的 scope 中去查找，一层层找，直到最顶层 scope (即 built-in scope, 其中放的都是 print 一类官方内置的变量或函数）。

python 默认有一个 built-in scope, 它是真正的全局作用域。往下一层是是 module 级别的作用域，名字叫做 global scope。global scope 并非名副其实的 "global"，即除了内置的函数和变量，其他所有自定义的函数和变量都无法跨 module 被查找到。global scope 再下面的作用域叫做 nonlocal scope.

如果想在一个函数中改变 global scope 中的变量，那么需要在访问这个变量时，加上 global 关键字，如

```python
message = "hello"
print(message)

def say():
    global message
    message = "bye"

say()
print(message)
```

还有一个关键字，叫 nonlocal，它与 global 关键字的区别在于，它只查找到 nonlocal scope 层级，不会在 global scope 中查找变量。


## 闭包 {#闭包}

闭包出现在函数嵌套的场景中，表示引用了其上层作用域中变量的嵌套函数。比如

```python
def multiplier(x):
    def multiply(y):
        return x * y
    return multiply

fn = multiplier(2)
print(fn(10))
```

由于 python 的局部作用域的机制，在调用完外层函数后，其包含的所有变量理应被删除，为了能让内部的函数使用，python 会创建一个临时变量，用于存储内部函数所需的变量，比如上面的例子，就创建了一个临时变量，用于存储 x。但是值得注意的是，需要外部函数被调用时，才会创建这个临时变量。看这个例子：

```python
multipliers = []

for i in range(1, 4):
    multipliers.append(lambda y: i * y)

m1, m2, m3 = multipliers
print(m1(10))
print(m2(10))
print(m3(10))
```

我们本来期望能输出 10, 20, 30，但是却全输出 30. 这就是因为，在 for 循环结束时，列表中的函数都还没有被调用过，循环中的变量 i=1,2 时，都没有被创建成临时变量保存起来，最后只保留了一个 i=3. 如果想要拿到循环中的变量，那就需要想办法在循环中触发嵌套函数的外层函数，使想要的变量被保存下来。比如

```python
multipliers = []

def multiplier(x):
    def multiply(y):
        return x * y
    return multiply

for i in range(1, 4):
    multipliers.append(multiplier(i))

m1, m2, m3 = multipliers
print(m1(10))
print(m2(10))
print(m3(10))
```
