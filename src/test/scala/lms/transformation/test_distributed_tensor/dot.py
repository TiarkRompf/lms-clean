import os
import torch
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    M = 32
    K = 13
    N = 16
    input = torch.randn(M, K)
    input.requires_grad = True
    weight = torch.randn(K, N)
    weight.requires_grad = True
    loss = torch.matmul(input, weight)
    loss.sum().backward()

    # printer
    printer = get_printer(lms_clean_root, test_name = "dot")
    printer("input", input, dim=0, degree=2)
    printer("weight", weight, dim=-1, degree=2)
    printer("loss", loss, dim=0, degree=2)
    printer("weight_grad", weight.grad, dim=-1, degree=2)
    printer("input_grad", input.grad, dim=0, degree=2)


if __name__ == "__main__":
    print(sys.argv)
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
