import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    dimX = 50
    dimY = 55
    dimZ = 60

    def generate_permute_data(permute):
        input = torch.randint(0, 10000, (dimZ, dimY, dimX))
        output = input.permute(permute[0], permute[1], permute[2])
        printer = get_printer(lms_clean_root, test_name=f"permute3D_{permute[0]}{permute[1]}{permute[2]}")
        printer("input.data", input)
        printer("output.data", output)

    generate_permute_data([0,2,1])
    generate_permute_data([1,2,0])
    generate_permute_data([2,1,0])
    generate_permute_data([2,0,1])


if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
