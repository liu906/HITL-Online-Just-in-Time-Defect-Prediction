from river import stream
from . import base
import pandas as pd
import os

class Commit_guru(base.FileDataset):
    def __init__(self, filename):

        df = pd.read_csv('datasets/' + filename)
        df.drop(['commit_hash', 'author_date_unix_timestamp', 'fixes', 'fix_unix_timestamp'], axis=1, inplace=True)
        super().__init__(
            filename=filename,
            n_samples=df.shape[0],
            n_features=14,
            task=base.BINARY_CLF
        )
    def __iter__(self):
        return stream.iter_csv(
            self.path,
            target="contains_bug",
            converters={'fix': lambda x: x == "TRUE",
                        'ns': float,
                        'nd': float,
                        'nf': float,
                        'entropy': float,
                        'la': float,
                        'ld': float,
                        'lt': float,
                        'ndev': float,
                        'age': float,
                        'nuc': float,
                        'exp': float,
                        'rexp': float,
                        'sexp': float, }
        )
