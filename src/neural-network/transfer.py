import numpy as np
from argparse import ArgumentParser

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("-s", "--source", type=str, help="source file")
    parser.add_argument("-d", "--destination", type=str, help="output file")

    args = parser.parse_args()

    np.save(args.destination, np.loadtxt(args.source))