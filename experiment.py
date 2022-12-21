import datetime

from model.AdaBoostClassifier import ada_boost_classifier
from model.ExtremelyFastDecisionTreeClassifier import extremely_fast_decision_tree_classifier
from model.BaggingClassifier import bagging_classifier
from model.AdaptiveRandomForestClassifier import adaptive_random_forest_classifier
from model.HoeffdingAdaptiveTreeClassifier import hoeffding_adaptive_tree_classifier
from river import metrics
from river import evaluate
from datasets.local_dataset import Commit_guru
from preprocess.scaler import *
from imblearn.imblearn import *
import argparse

ADABOOSTCLASSIFER = 'AdaBoostClassifer'
EXTREMELYFASTDECISIONTREECLASSIFIER = 'ExtremelyFastDecisionTreeClassifier'
BAGGINGCLASSIFIER = 'BaggingClassifier'
ADAPTIVERANDOMFORESTCLASSIFIER = 'AdaptiveRandomForestClassifier'
HOEFFDINGADAPTIVETREECLASSIFIER = 'HoeffdingAdaptiveTreeClassifier'


MINMAXSCALER = 'MinMaxScaler'
STANDARDSCALER = 'StandardScaler'

RANDOMSAMPLER = 'RandomSampler'
RANDOMUNDERSAMPLER = 'RandomUnderSampler'
RANDOMOVERSAMPLER = 'RandomOverSampler'

def clf_match(str_clf):
    if str_clf == ADABOOSTCLASSIFER:
        model = ada_boost_classifier()
    elif str_clf == EXTREMELYFASTDECISIONTREECLASSIFIER:
        model = extremely_fast_decision_tree_classifier()
    elif str_clf == BAGGINGCLASSIFIER:
        model = bagging_classifier()
    elif str_clf == ADAPTIVERANDOMFORESTCLASSIFIER:
        model = adaptive_random_forest_classifier()
    elif str_clf == HOEFFDINGADAPTIVETREECLASSIFIER:
        model = hoeffding_adaptive_tree_classifier()

    else:
        print('no classifier can match the str_clf!')
        exit(-1)

    return model


def scaler_match(str_scaler):
    if str_scaler == MINMAXSCALER:
        scaler = min_max_scaler()
    elif str_scaler == STANDARDSCALER:
        scaler = standard_scaler()
    elif str_scaler == '':
        scaler = None
    else:
        print('no scaler can match the str_scaler!')
        exit(-1)

    return scaler



def imb_match(str_imb,model):
    if str_imb == RANDOMSAMPLER:
        model = random_sampler(model)
    elif str_imb == RANDOMOVERSAMPLER:
        model = random_over_sampler(model)
    elif str_imb == RANDOMUNDERSAMPLER:
        model = random_under_sampler(model)
    elif str_imb == '':
        model = model
    else:
        print('no imblearn model can match the str_imb!')
        exit(-1)

    return model


if __name__ == '__main__':
    arg = argparse.ArgumentParser()
    '''
    experiment -clf ExtremelyFastDecisionTreeClassifier -scaler StandardScaler -imb RandomOverSampler -delay 129600 -f test.csv
    '''
    arg.add_argument('-clf', type=str, help='classifier in river, e.g. HoeffdingAdaptiveTreeClassifier', required=True)
    arg.add_argument('-scaler', type=str, default='',
                     help='scaler in river, e.g. StandardScaler')
    arg.add_argument('-imb', type=str, default='',
                     help='imbalance algorithm in river, e.g. RandomOverSampler')
    arg.add_argument('-delay', type=int, default=0,
                     help='validation delay seconds. e.g. 129600 , means 90 days')
    arg.add_argument('-f', type=str,
                     help='filename used to build model. the file must be put under datasets/ e.g., test.csv')

    args = arg.parse_args()
    clf = clf_match(args.clf)
    scaler = scaler_match(args.scaler)
    str_imb = args.imb
    delay = args.delay
    f = args.f


    if scaler is None:
        model = imb_match(str_imb, model=clf)
    else:
        model = imb_match(str_imb, model=scaler | clf)

    filename = clf + '_' + scaler + '_' + str_imb + '_' + str(delay) + '_' + f


    dataset = Commit_guru(filename=filename)
    if delay == 0:
        with open('log/' + filename+'.log', 'w') as f:
            evaluate.progressive_val_score(
                model=model,
                dataset=dataset,
                metric=metrics.ClassificationReport(),
                print_every=400,
                file=f
            )
    else:
        with open('log/' + filename+'.log', 'w') as f:
            evaluate.progressive_val_score(
                model=model,
                dataset=dataset,
                metric=metrics.ClassificationReport(),
                moment='author_date_unix_timestamp',
                delay=datetime.timedelta(seconds=delay),
                print_every=400,
                file=f
            )

    # dataset = Commit_guru(filename='129600_test.csv')
    #
    # with open('129600_test.log', 'w') as f:
    #     evaluate.progressive_val_score(
    #         model=model,
    #         dataset=dataset,
    #         metric=metrics.ClassificationReport(),
    #         moment='author_date_unix_timestamp',
    #         delay=datetime.timedelta(seconds=129600),
    #         print_every=400,
    #         file=f
    #     )

    # dataset = Commit_guru(filename='0_test.csv')
    #
    # with open('0_test.log', 'w') as f:
    #     evaluate.progressive_val_score(
    #         model=model,
    #         dataset=dataset,
    #         metric=metrics.ClassificationReport(),
    #         print_every=400,
    #         file=f
    #     )
    # print(evaluate.progressive_val_score(dataset, model, metric))


#TODO: 1.prequential evaluation
#      2.save detailed prediction result
#      3.clutering algorithm
