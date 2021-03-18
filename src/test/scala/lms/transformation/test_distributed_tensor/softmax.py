import os
import torch
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    input = torch.zeros(2, 1, 3, 3)
    input.requires_grad = True

    zeros = [1] * (2*3*3)
    zeros[0] = 0
    temp = torch.tensor(zeros, dtype=torch.float32)
    weight = torch.reshape(temp, (2, 1, 3, 3))
    weight.requires_grad = True
    m = torch.nn.Softmax(dim=0)
    loss = input + m(weight)
    loss.sum().backward()

    # printer
    printer = get_printer(lms_clean_root, test_name = "softmax")
    printer("input", input, dim=0, degree=2)
    printer("weight", weight, dim=0, degree=2)
    printer("loss", loss, dim=0, degree=2)
    printer("weight_grad", weight.grad, dim=0, degree=2)
    printer("input_grad", input.grad, dim=0, degree=2)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
