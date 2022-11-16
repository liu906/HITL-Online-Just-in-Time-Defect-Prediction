from river import ensemble
from river import evaluate
from river import metrics
from river.datasets import synth

def adaptive_random_forest_classifier():
    model = ensemble.AdaptiveRandomForestClassifier(seed=8, leaf_prediction="mc")
    # metric = metrics.Recall()
    # print(evaluate.progressive_val_score(dataset, model, metric))
    return model

def test():
    from datasets.local_dataset import Commit_guru
    dataset = Commit_guru(filename='test.csv')
    adaptive_random_forest_classifier(dataset, metric_list=[])

if __name__=='__main__':
    test()