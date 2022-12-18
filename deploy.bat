hugo -D

git add .

SET msg="updating site %date%"

git commit -m %msg%

git push origin master
