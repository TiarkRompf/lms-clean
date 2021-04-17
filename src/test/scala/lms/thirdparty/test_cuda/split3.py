import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    d0 = 2
    d1 = 2
    d2 = 2+2+1

    input = torch.randn(d0, d1, d2)
    output0, output1, output2 = torch.split(input, [2,2,1], 2)
    output = torch.cat((input0, output1, output2), 0)

    # printer
    printer = get_printer(lms_clean_root, test_name = "split3")
    printer("input.data", input)
    printer("output.data", output)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
