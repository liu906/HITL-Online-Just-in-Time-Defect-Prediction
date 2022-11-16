from river.datasets import synth
from river import evaluate
from river import metrics
from river import tree


def extremely_fast_decision_tree_classifier():
    model = tree.ExtremelyFastDecisionTreeClassifier(
        grace_period=100,
        delta=1e-5,
        nominal_attributes=['elevel', 'car', 'zipcode'],
        min_samples_reevaluate=100
    )

    # metric = metrics.Recall()
    #
    # print(evaluate.progressive_val_score(dataset, model, metric))
    return model


def test():
    from datasets.local_dataset import Commit_guru
    dataset = Commit_guru(filename='test.csv')
    extremely_fast_decision_tree_classifier(dataset, metric_list=[])


if __name__ == '__main__':
    test()
