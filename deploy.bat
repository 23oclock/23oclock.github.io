hugo -D

git add .

SET msg="rebuilding site %date%"

git commit -m %msg%

git push origin master
