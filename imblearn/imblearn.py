from river import imblearn


def random_over_sampler(model, desired_dist={False: 0.5, True: 0.5}, seed=42):
    model = imblearn.RandomOverSampler(
        (
            model
        ),
        desired_dist=desired_dist,
        seed=seed
    )
    return model


def random_under_sampler(model, desired_dist={False: 0.5, True: 0.5}, seed=42):
    model = imblearn.RandomUnderSampler(
        (
            model
        ),
        desired_dist=desired_dist,
        seed=seed
    )
    return model


def random_sampler(model, desired_dist={False: 0.5, True: 0.5}, sampling_rate=0.8, seed=42):
    model = imblearn.RandomSampler(
        (
            model
        ),
        desired_dist=desired_dist,
        sampling_rate=sampling_rate,
        seed=seed
    )
    return model
