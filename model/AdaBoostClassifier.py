from river import datasets
from river import ensemble
from river import evaluate
from river import metrics
from river import tree

def ada_boost_classifier():


    model = ensemble.AdaBoostClassifier(
        model=(
            tree.HoeffdingTreeClassifier(
                split_criterion='gini',
                delta=1e-5,
                grace_period=2000
            )
        ),
        n_models=5,
        seed=42
    )
    return model

def test():
    from datasets.local_dataset import Commit_guru
    dataset = Commit_guru(filename='test.csv')
    ada_boost_classifier(dataset, metric_list=[])


if __name__=='__main__':
    test()