import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    dimX = 600
    dimY = 60
    dimZ = 40
    input = torch.randint(0, 10000, (dimZ, dimY, dimX))
    output = input.permute(1, 0, 2)

    # printer
    printer = get_printer(lms_clean_root, test_name = "permute_kernel_102_big")
    printer("input.data", input)
    printer("output.data", output)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
