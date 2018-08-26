import numpy as np
import config
import os


def load_train_data(file_name):
    """
    Load data from file.

    :param file_name: path of data file
    :return: numpy array
    """

    return np.load(file_name)


def generate_train_data(
        racket_cmd=config.default_racket_cmd,
        herbie_root=os.getcwd(),
        output_dir=config.default_data_dir,
        n_threads=config.default_n_threads,
        timeout=config.default_timeout):
    """
    Extract training data by running herbie benchmark

    :param racket_cmd: path of racket command
    :param output_dir: output directory of data file
    :param n_threads:  number of threads to use for generating data
    :param timeout:    timeout(seconds) for each test
    :return: None
    """

    # change current working directory to herbie root directory
    os.chdir("../..")

    herbie_rkt = herbie_root + "/src/herbie.rkt"
    bench_dir = herbie_root + "/bench/"
    temp_dir = output_dir + "/temp"
    data_file = output_dir + "/data.txt"

    if not os.path.exists(output_dir):
        os.mkdir(output_dir)

    if not os.path.exists(temp_dir):
        os.mkdir(temp_dir)

    if not os.path.exists(data_file):
        os.system("touch " + data_file)

    # <racket-cmd> <herbie.rkt> report --timeout <N> --threads <N> --seed <S> <input> <output>
    timeout_opt = "--timeout " + str(timeout)
    threads_opt = "--threads " + str(n_threads)
    trace_opt = "--trace "
    seed_opt = "--seed " + config.default_seed
    options = timeout_opt + " " + threads_opt + " " + trace_opt + " " + seed_opt

    report_cmd = racket_cmd + " " + herbie_rkt + " report"
    report_cmd = report_cmd + " " + options + " " + bench_dir + " " + temp_dir
    print("Herbie benchmark cmd:")
    print(report_cmd)

    # find <temp_dir> -name 'trace.txt' | xargs -I {} cat {} >> <output>
    extract_cmd = "find " + temp_dir + " -name 'trace.txt' | xargs -I {} cat {} >> " + data_file
    print("Data extraction cmd:")
    print(extract_cmd)

    # python3 transfer.py -s <input_file> -d <ouput_file>
    transfer_file = herbie_root + "/src/neural-network/transfer.py"
    transfer_cmd = "python3 " + transfer_file + " -s " + data_file + " -d " + output_dir + "/data.npy"
    print("Data transfer cmd:")
    print(transfer_cmd)

    # run herbie benchmark and save data
    shell = report_cmd + " && " + extract_cmd + " && " + transfer_cmd
    os.system(shell)

    # rm -rf <temp_dir> <data_file>
    remove_cmd = "rm -rf " + temp_dir + " " + data_file
    print("File deletion cmd:")
    print(remove_cmd)

    # remove unused files
    os.system(remove_cmd)
