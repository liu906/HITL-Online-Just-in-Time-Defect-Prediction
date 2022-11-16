from river import datasets
from river import ensemble
from river import evaluate
from river import linear_model
from river import metrics
from river import optim
from river import preprocessing




def bagging_classifier():
    model = ensemble.BaggingClassifier(
        model=(
                preprocessing.StandardScaler() |
                linear_model.LogisticRegression()
        ),
        n_models=3,
        seed=42
    )

    # metric = metrics.Recall()
    #
    # print(evaluate.progressive_val_score(dataset, model, metric))
    # print(model)
    return model

def test():
    from datasets.local_dataset import Commit_guru
    dataset = Commit_guru(filename='test.csv')
    bagging_classifier(dataset, metric_list=[])

if __name__=='__main__':
    test()