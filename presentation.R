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
  labs(title='position="fill"')

ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position = "identity",alpha=0.3)+
  labs(title='position="identity"')



