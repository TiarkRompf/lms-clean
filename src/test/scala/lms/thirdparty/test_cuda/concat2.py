import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    input0 = torch.randn(2, 3, 3)
    input1 = torch.randn(2, 3, 5)
    output = torch.cat((input0, input1), 2)

    # printer
    printer = get_printer(lms_clean_root, test_name = "concat")
    printer("input0.data", input0)
    printer("input1.data", input1)
    printer("output.data", output)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
