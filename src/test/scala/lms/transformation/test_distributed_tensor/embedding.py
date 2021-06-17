import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer, get_int_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    n_embedding = 8
    embed_size = 5
    embed = nn.Embedding(n_embedding, embed_size)

    n_indices = 4
    indices = torch.randint(0, n_embedding, (n_indices,))

    loss = embed(indices)
    loss.retain_grad()
    loss.sum().backward()

    # printer
    printer = get_printer(lms_clean_root, test_name = "embedding")
    int_printer = get_int_printer(lms_clean_root, test_name = "embedding")
    printer("embed", embed.weight, dim=-1, degree=2)
    int_printer("indices", indices, dim=0, degree=2)
    printer("loss", loss, dim=0, degree=2)
    printer("embed_grad", embed.weight.grad, dim=-1, degree=2)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
