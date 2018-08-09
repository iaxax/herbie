import tensorflow as tf
import numpy as np
import config
import dataset
import util


def cnn_model_fn(features, labels, mode):
    """ Model function for CNN """

    # Input layer
    input_layer = tf.reshape(features["x"], [-1, 32, 16, 1])
    padding = tf.constant([[0, 0], [2, 2], [2, 2], [0, 0]])
    input_layer = tf.pad(input_layer, padding, "SYMMETRIC")

    # Convolutional layer #1
    convolution_layer_1 = tf.layers.conv2d(
        inputs=input_layer,
        filters=32,
        kernel_size=[5, 5],
        padding="valid",
        activation=tf.nn.relu6
    )

    # Pooling layer #1
    pool1 = tf.layers.max_pooling2d(inputs=convolution_layer_1, pool_size=[2, 2], strides=2)
    pool1 = tf.pad(pool1, padding, "SYMMETRIC")

    # Convolution layer #2
    convolution_layer_2 = tf.layers.conv2d(
        inputs=pool1,
        filters=64,
        kernel_size=[5, 5],
        padding="valid",
        activation=tf.nn.relu
    )

    # Pooling layer #2
    pool2 = tf.layers.max_pooling2d(inputs=convolution_layer_2, pool_size=[2, 2], strides=2)

    # Dense layer #1
    pool2_flat = tf.reshape(pool2, [-1, 8 * 4 * 64])
    dense1 = tf.layers.dense(inputs=pool2_flat, units=2048, activation=tf.nn.relu)
    dropout1 = tf.layers.dropout(
        inputs=dense1, rate=0.4, training=mode == tf.estimator.ModeKeys.TRAIN
    )

    # Dense layer #2
    dense2 = tf.layers.dense(inputs=dropout1, units=2048, activation=tf.nn.relu)
    dropout2 = tf.layers.dropout(
        inputs=dense2, rate=0.4, training=mode == tf.estimator.ModeKeys.TRAIN
    )

    # Logits layer
    logits = tf.layers.dense(inputs=dropout2, units=1024, activation=tf.nn.sigmoid)

    predictions = {
        # Generate predictions (for PREDICT mode)
        "predictions": logits,
        # Generate rule and location (for EVAL mode)
        "classes": util.decode_one_labels(logits),
        # Add `relu_tensor`to the graph. It is used for PREDICT and by the `logging_hook`
        "probabilities": tf.nn.relu(logits, name="relu_tensor")
    }

    if mode == tf.estimator.ModeKeys.PREDICT:
        return tf.estimator.EstimatorSpec(mode=mode, predictions=predictions)

    # Calculate loss (for both TRAIN and EVAL modes)
    loss = tf.losses.mean_squared_error(labels=labels, predictions=logits)

    # Configure the training op (for TRAIN mode)
    if mode == tf.estimator.ModeKeys.TRAIN:
        optimizer = tf.train.GradientDescentOptimizer(learning_rate=config.learning_rate)
        train_op = optimizer.minimize(
            loss=loss,
            global_step=tf.train.get_global_step()
        )
        return tf.estimator.EstimatorSpec(mode=mode, loss=loss, train_op=train_op)

    # Add evaluation metrics (for EVAL mode)
    eval_metric_ops = {
        "accuracy": tf.metrics.accuracy(
            labels=util.decode_one_labels(labels), predictions=predictions["classes"]
        )
    }
    return tf.estimator.EstimatorSpec(mode=mode, loss=loss, eval_metric_ops=eval_metric_ops)


def predict(x):
    """
    Feed data into neural network and predict a rule and a location

    :param x: A list which consists of encoding expression and floating points

    :return: {'rule': <list>, 'location': <int>}
             rule is a probability distribution
             location is an encoded integer
    """

    predict_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={"x": np.resize(x, [-1, config.x_size])},
        num_epochs=config.predict_epochs,
        shuffle=False
    )

    predictions = classifier.predict(
        input_fn=predict_input_fn
    )

    result = []
    for p in predictions:
        y = p["probabilities"]
        rule, location = y[:-1], y[-1]
        result.append({"rule": rule, "location": location})

    return result


def train(train_data_file):
    """
    Train a CNN to predict rule and location

    :param train_data_file:

    :return: None
    """

    # Open log
    tf.logging.set_verbosity(tf.logging.INFO)

    # Load training and eval data
    train_data = dataset.load_train_data(train_data_file)

    # for x, y in train_data:
    # Set up logging for prediction
    tensors_to_log = {"probabilities": "relu_tensor"}
    logging_hook = tf.train.LoggingTensorHook(
        tensors=tensors_to_log, every_n_iter=config.log_every_n_iter
    )

    # Train the model
    x, y = train_data[:, :config.x_size], train_data[:, config.x_size:]
    train_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={"x": x},
        y=util.encode_one_hot_labels(y),
        batch_size=config.batch_size,
        num_epochs=None,
        shuffle=True
    )
    classifier.train(
        input_fn=train_input_fn,
        steps=config.steps,
        hooks=[logging_hook]
    )


def evaluate(test_data_file):
    """
    Evaluate the accuracy of the model

    :return: None
    """

    # Load training and eval data
    train_data = dataset.load_train_data(test_data_file)

    # Evaluate the model
    x, y = train_data[:, :config.x_size], train_data[:, config.x_size:]
    eval_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={"x": x},
        y=util.encode_one_hot_labels(y),
        num_epochs=config.eval_epochs,
        shuffle=False
    )
    result = classifier.evaluate(
        input_fn=eval_input_fn,
    )
    print(result)


# Create a estimator
classifier = tf.estimator.Estimator(
    model_fn=cnn_model_fn,
    model_dir="cnn_model"
)
