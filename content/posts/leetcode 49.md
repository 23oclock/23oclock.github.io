+++
title = "leetcode 49. Group Anagrams"
author = ["flumine@qq.com"]
date = 2022-12-18T23:32:00+08:00
lastmod = 2022-12-18T23:32:23+08:00
tags = ["leetcode"]
categories = ["技术"]
draft = false
+++

题目描述：本题输入是一些词语，要求把所有的异位词 (anagram) 分组。所谓异位词就是组成元素及其个数都相同的单词。
解决方法：定义一种编码方式，使得所有的异位词编码都相同，并用这个编码作为字典的 key, 这样就可以把编码相同的词（异位词）都放到一个 key 下，最后返回字典的 values 即可。
代码：

```python
def groupAnagrams(strs: List[str]) -> List[List[str]]:
  res = defaultdict(list)
  for s in strs:
    count = [0] * 26
    for c in s:
      count[ord(c) - ord("a")] += 1
      res[tuple(count)].append(s)

  return res.values()
```
