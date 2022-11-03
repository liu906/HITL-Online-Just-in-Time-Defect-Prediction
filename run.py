from datasets.local_dataset import Commit_guru

tomcat = Commit_guru(filename='test.csv')

x, y = next(iter(tomcat))
print(x)
print(y)
from river import linear_model
model = linear_model.LogisticRegression()
model.predict_proba_one(x)
model = model.learn_one(x,y)
for x,y in tomcat:
    print(x,y)
