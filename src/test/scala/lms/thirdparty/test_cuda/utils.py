import os
import torch

def extend(dirname: str, extension: str):
    """
    extend dir name with extension.
    """
    dirname = os.path.join(dirname, extension, "")
    if not os.path.exists(dirname):
        os.mkdir(dirname)
    return dirname

# save tensor to file
def get_printer(lms_clean_root: str, test_name: str):
    def print_flatten_array(name: str, tensor: "torch.tensor"):
        sub_dir = "src/out/"
        under = "thirdparty/cuda/golden/"
        dirname = extend(os.path.join(lms_clean_root, sub_dir), under)
        dirname = extend(dirname, test_name)
        with open(os.path.join(dirname, name), "w") as f:
            print(os.path.join(dirname, name))
            for data in tensor.flatten():
                f.write(f"{float(data)} ")

    return print_flatten_array

def get_int_printer(lms_clean_root: str, test_name: str):
    def print_flatten_array(name: str, tensor: "torch.tensor"):
        sub_dir = "src/out/"
        under = "thirdparty/cuda/golden/"
        dirname = extend(os.path.join(lms_clean_root, sub_dir), under)
        dirname = extend(dirname, test_name)
        with open(os.path.join(dirname, name), "w") as f:
            print(os.path.join(dirname, name))
            for data in tensor.flatten():
                f.write(f"{int(data)} ")

    return print_flatten_array
