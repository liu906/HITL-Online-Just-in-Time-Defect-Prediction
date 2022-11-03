from river import stream
from . import base
import pandas as pd

class Commit_guru(base.FileDataset):
    def __init__(self, filename):
        df = pd.read_csv(filename)
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
            converters={'fix': int, 'ns': int, 'nd': int, 'nf': int, 'entropy': int, 'la': int, 'ld': int, 'lt': int,
                        'ndev': int, 'age': int, 'nuc': int, 'exp': int, 'rexp': int, 'sexp': int,
                        }
        )