import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    input = torch.randn(214, 56)
    input.requires_grad = True
    loss = torch.transpose(input, 0, 1)
    loss.sum().backward()

    # printer
    printer = get_printer(lms_clean_root, test_name = "transpose")
    printer("input", input, dim=0, degree=2)
    printer("loss", loss, dim=1, degree=2)
    printer("input_grad", input.grad, dim=0, degree=2)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
