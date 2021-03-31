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

        if tensor.dtype == torch.float32 or tensor.dtype == torch.float64 or tensor.dtype == torch.float16:
            with open(os.path.join(dirname, name), "w") as f:
                print(os.path.join(dirname, name))
                for data in tensor.flatten():
                    f.write(f"{float(data)} ")
        elif tensor.dtype == torch.int8 or tensor.dtype == torch.int16 or tensor.dtype == torch.int32 or tensor.dtype == torch.int64:
            with open(os.path.join(dirname, name), "w") as f:
                print(os.path.join(dirname, name))
                for data in tensor.flatten():
                    f.write(f"{int(data)} ")
        else:
            raise RuntimeError(f"torch tensor type {tensor.dtype} not yet handled")

    return print_flatten_array
