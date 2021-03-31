import os
import torch
import torch.nn as nn
import sys
from utils import extend, get_printer, get_int_printer


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
    int_printer = get_int_printer(lms_clean_root, test_name = "embedding")
    printer("embedding.data", embedding.weight)
    int_printer("indices.data", indices)
    printer("output.data", output)

if __name__ == '__main__':
    assert len(sys.argv) > 1, "must provide lms_clean_root dir"
    generate_data(sys.argv[1])
