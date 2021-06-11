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
    dimZ = 80

    input = torch.randn(dimZ, dimY, dimX)
    input.requires_grad = True
    loss = input.permute(1, 0, 2)
    loss.sum().backward()

    printer = get_printer(lms_clean_root, test_name=f"permute3D_{permute[0]}{permute[1]}{permute[2]}")
    printer("input", input, dim=0, degree=2)
    printer("loss", loss, dim=1, degree=2)
    printer("input_grad", input.grad, dim=0, degree=2)


if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
