from datasets.local_dataset import Commit_guru

tomcat = Commit_guru(filename='tomcat(master).csv')

x,y=next(iter(tomcat))
print(x)
print(y)