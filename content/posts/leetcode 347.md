+++
title = "leetcode 347. Top K Frequent Elements"
author = ["flumine@qq.com"]
date = 2022-12-18T23:53:00+08:00
lastmod = 2022-12-19T00:35:36+08:00
tags = ["leetcode"]
categories = ["技术"]
draft = false
+++

问题描述：给你一个数组，数组里面有一些整数，请返回出现次数排在前 k 的整数。

解决方法：使用首先使用一个字典统计整数出现的频次，再使用一个数组（本质是字典）存储每个频次对应的那些整数，最后从数组中由后往前找到前 k 个高频整数。

代码：

```python
def topKFrequent(nums: List[int], k: int) -> List[int]:
    count = {}
    for x in nums:
        count[x] = 1 + count.get(x, 0)

        freq = [[] for _ in range(len(nums) + 1)]
        for x, n in count.items():
            freq[n].append(x)

    res = []
    for i in range(len(freq) - 1, -1, -1):
        for x in freq[i]:
            res.append(x)
            if len(res) == k:
                return res
```
