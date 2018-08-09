import tensorflow as tf
import numpy as np
import config


def encode_one_hot_labels(labels):
    """
    Make rule in one-hot style and keep location as an int

    :param labels: a 2D numpy array consisting of rule(int) and location(int)

    :return: a 2D numpy array
    """

    one_hot_labels = []
    identity_matrix = np.eye(config.rule_max_num)

    for label in labels:
        rule, location = label[0], label[1]
        one_hot_rule = identity_matrix[int(rule)]
        one_hot_labels.append(np.append(one_hot_rule, location))

    return np.array(one_hot_labels)


def decode_one_labels(labels):
    """
    Decode rule from one-hot style and keep location as an int

    :param labels: a 2D tensor consisting of rule(list) and location(int)

    :return: a 2D tensor
    """

    rule = tf.argmax(labels[:, :-1], axis=1)
    rule = tf.reshape(rule, [-1, 1])

    location = tf.cast(labels[:, -1], tf.int64)
    location = tf.reshape(location, [-1, 1])

    return tf.concat([rule, location], axis=1)


def tensor_logit(t):
    """
    Logit function i.e. inverse function of sigmoid
    Logit(x) = log(x/(1-x))

    Note: final result is casted to tf.int64 instead of keeping tf.float64

    :param t: tensor
    :return: logit(t)
    """

    one = tf.constant(1.0, dtype=tf.float64)
    logit = tf.log(tf.div(t, tf.subtract(one, t)))
    return tf.cast(logit, dtype=tf.int64)
