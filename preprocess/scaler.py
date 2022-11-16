from river import preprocessing
from river import base
def standard_scaler():
    scaler = preprocessing.StandardScaler()
    return scaler

def min_max_scaler():
    scaler = preprocessing.MinMaxScaler()
    return scaler

def normalizer():
    scaler = preprocessing.Normalizer(order=2)
    return scaler

def base_scaler():
    scaler = base.Transformer()
    return scaler