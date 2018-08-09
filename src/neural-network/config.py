import os

# file of training data
train_data_file = os.getcwd() + "/data/data.npy"

# file of testing data
test_data_file = os.getcwd() + "/data/data.npy"

# maximum number of rules
rule_max_num = 1023

# number of neurons in input layer
x_size = 512

# learning rate of optimizer
learning_rate = 0.0001

# batch size of training data
batch_size = 100

# number of iterations in training
steps = 5000

# frequency of logging
log_every_n_iter = 50

# number of epochs in evaluation
eval_epochs = 1

# number of epochs in prediction
predict_epochs = 1

# number of training data each time we feed into model
chunk_size = 100

# host of the socket server
socket_host = "localhost"

# port on which the socket server is listening
socket_port = 4399

# default number of threads for generating data
default_n_threads = 1

# default timeout(seconds) for each test
default_timeout = 600

# default directory in which training data locates
default_data_dir = os.getcwd() + "/data"

# default racket command
default_racket_cmd = "racket"

# default seed for racket benchmark
default_seed = "'#(2775764126 3555076145 3898259844 1891440260 2599947619 1948460636)'"

# data encoding
encoding = "utf8"
