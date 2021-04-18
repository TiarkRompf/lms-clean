import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer


def generate_data(lms_clean_root: str):
    torch.manual_seed(0)

    # model
    n_embedding = 20
    embed_size = 60
    embedding = nn.Embedding(n_embedding, embed_size)

    n_indices = 10
    indices = torch.randint(0, n_embedding, (n_indices,))

    output = embedding(indices)

    # printer
    printer = get_printer(lms_clean_root, test_name = "embedding")
    printer("embedding.data", embedding.weight)
    printer("indices.data", indices)
    printer("output.data", output)
    printer("input_grad.data", input.grad)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
