import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # compute softmax forward and backward
    shape = (32, 533)
    input = torch.randn(shape)
    input.requires_grad = True
    m = torch.nn.LogSoftmax(dim=1)
    output = m(input)
    output.retain_grad()
    w = torch.nn.Linear(shape[1], 1)
    y = w(output)
    y.sum().backward()

    # printer
    printer = get_printer(lms_clean_root, test_name = "logSoftmax")
    printer("input.data", input)
    printer("output.data", output)
    printer("input_grad.data", input.grad)
    printer("output_grad.data", output.grad)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
