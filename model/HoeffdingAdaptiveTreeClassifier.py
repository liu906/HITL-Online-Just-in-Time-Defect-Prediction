from river.datasets import synth
from river import evaluate
from river import metrics
from river import tree


def hoeffding_adaptive_tree_classifier():
    model = tree.HoeffdingAdaptiveTreeClassifier(
        grace_period=100,
        delta=1e-5,
        leaf_prediction='nb',
        nb_threshold=10,
        seed=0
    )

    return model
