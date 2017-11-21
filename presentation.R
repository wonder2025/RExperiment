library(ggplot2)
data(Salaries,package="car")
dim(Salaries)

#fill=rank：以rank进行分组，alpha=.3：图形的透明度
ggplot(Salaries,aes(x=salary,fill=rank))+ 
  geom_density(alpha=.3)
#
ggplot(Salaries,aes(x= yrs.since.phd,y=salary,color=rank,shape=sex))+
  geom_point()
#可以看见总人数的对比
ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position = "stack")+
  labs(title='position="stack"')

ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position = "dodge")+
  labs(title='position="dodge"')

ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position = "fill")+
  labs(title='position="fill"',y="proportion")

ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position = "identity",alpha=0.3)+
  labs(title='position="identity"')

#构建不同于mtcars的数据集mtcars.c
mtcars.c <- transform(mtcars, mpg = mpg^2)
ggplot()+
  geom_point(aes(x = hp, y = mpg), data = mtcars, color = "red") + 
  geom_point(aes(x = mtcars$hp, y = mtcars$disp), color = "green")+ 
  #选用向量数据
  geom_point(aes(x = hp, y= mpg), data = mtcars.c, color = "blue") 
#选用不同的数据集



