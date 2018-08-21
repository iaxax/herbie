import socketserver
import config
import json
import model
import dataset
from argparse import ArgumentParser

class ServiceHandler(socketserver.StreamRequestHandler):
    """
    A Socket Server
    """

    def handle(self):
        # Extract input data
        data = self.rfile.readline().strip()
        json_data = json.loads(str(data, encoding=config.encoding))

        # Make a prediction
        prediction = model.predict(json_data)
        json_prediction = json.dumps(prediction)

        # Send back the result
        self.wfile.write(bytes(json_prediction, encoding=config.encoding))
        self.wfile.flush()


if __name__ == '__main__':
    # Config command line options
    parser = ArgumentParser()
    sub_parser = parser.add_subparsers(dest="sub_cmd")

    socket_parser = sub_parser.add_parser(
        name="socket", description="Set up a socket server for model prediction"
    )

    train_parser = sub_parser.add_parser(name="train", description="Train the model")
    train_parser.add_argument("-f", "--file", type=str, help="file of training data")

    eval_parser = sub_parser.add_parser(name="evaluate", description="Evaluate the model")
    eval_parser.add_argument("-f", "--file", type=str, help="file of testing data")

    data_parser = sub_parser.add_parser(name="data", description="Generating training data")
    data_parser.add_argument("-t", "--threads", type=int, help="number of threads to use for generating data")
    data_parser.add_argument("-T", "--timeout", type=int, help="timeout(seconds) for each test")
    data_parser.add_argument("-d", "--destination", type=str, help="location to store the data file")
    data_parser.add_argument("-r", "--racket", type=str, help="path of racket")

    # Parse arg
    args = parser.parse_args()

    if args.sub_cmd == 'socket':
        # Set up a socket server
        server = socketserver.ThreadingTCPServer((config.socket_host, config.socket_port), ServiceHandler)
        print("Server running...")
        server.serve_forever()

    elif args.sub_cmd == 'train':
        # Generate training data
        n_threads = args.threads if args.threads else config.default_n_threads
        timeout = args.timeout if args.timeout else config.default_timeout
        output_dir = args.destination if args.destination else config.default_data_dir
        racket_cmd = args.racket if args.racket else config.default_racket_cmd
        dataset.generate_train_data(racket_cmd, output_dir, n_threads, timeout)

    elif args.sub_cmd == 'evaluate':
        # Train the model
        train_data_file = args.file if args.file else config.train_data_file
        model.train(train_data_file)

    elif args.sub_cmd == 'data':
        # Evaluate the model
        test_data_file = args.file if args.file else config.test_data_file
        model.evaluate(test_data_file)

    else:
        parser.print_help()
