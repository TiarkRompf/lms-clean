import os
import torch
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    input = torch.zeros(2, 1, 9, 9)
    input.requires_grad = True
    temp = torch.tensor(list(range(0, 2*9*9)), dtype=torch.float32)
    weight = torch.reshape(temp, (2, 1, 9, 9))
    weight.requires_grad = True
    m = torch.nn.AvgPool2d(kernel_size=3, padding=1, stride=1, count_include_pad=True)
    loss = input + m(weight)
    loss.sum().backward()

    # printer
    printer = get_printer(lms_clean_root, test_name = "maxpool")
    printer("input", input, dim=0, degree=2)
    printer("weight", weight, dim=0, degree=2)
    printer("loss", loss, dim=0, degree=2)
    printer("weight_grad", weight.grad, dim=0, degree=2)
    printer("input_grad", input.grad, dim=0, degree=2)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
