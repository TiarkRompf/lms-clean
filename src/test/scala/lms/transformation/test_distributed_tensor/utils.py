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
        under = "transformer/distributed_tensor/"
        dirname = extend(os.path.join(lms_clean_root, sub_dir), under)
        dirname = extend(dirname, test_name)
        dirname = extend(dirname, "golden")
        with open(os.path.join(dirname, name), "w") as f:
            print(os.path.join(dirname, name))
            for data in tensor.flatten():
                f.write(f"{float(data)} ")

    def ranked_print_flatten_array(name: str, tensor: "torch.tensor", dim: int, degree: int):
        if dim == 0:
            size = list(tensor.size())[0]
            assert size % degree == 0, "should be divisible"
            slice = size // degree
            for i in range(degree):
                print_flatten_array(name+"_rank_"+str(i)+".data", tensor[slice * i : slice * (i + 1)])
        elif dim == -1:
            for i in range(degree):
                print_flatten_array(name+"_rank_"+str(i)+".data", tensor)
        elif dim == 1:
            shape = list(tensor.size())
            assert len(shape) > 1, "should be at least 2D"
            size = shape[1]
            assert size % degree == 0, "should be divisible"
            slice = size // degree
            for i in range(degree):
                print_flatten_array(name+"_rank_"+str(i)+".data", tensor[:, slice * i : slice * (i + 1)])
        elif dim == 2:
            shape = list(tensor.size())
            assert len(shape) > 2, "should be at least 2D"
            size = shape[2]
            assert size % degree == 0, "should be divisible"
            slice = size // degree
            for i in range(degree):
                print_flatten_array(name+"_rank_"+str(i)+".data", tensor[:, :, slice * i : slice * (i + 1)])
        else:
            assert False, "TODO"

    return ranked_print_flatten_array

# save tensor to file
def get_int_printer(lms_clean_root: str, test_name: str):

    def print_flatten_array_int(name: str, tensor: "torch.tensor"):
        sub_dir = "src/out/"
        under = "transformer/distributed_tensor/"
        dirname = extend(os.path.join(lms_clean_root, sub_dir), under)
        dirname = extend(dirname, test_name)
        dirname = extend(dirname, "golden")
        with open(os.path.join(dirname, name), "w") as f:
            print(os.path.join(dirname, name))
            for data in tensor.flatten():
                f.write(f"{int(data)} ")

    def ranked_print_flatten_array_int(name: str, tensor: "torch.tensor", dim: int, degree: int):
        if dim == 0:
            size = list(tensor.size())[0]
            assert size % degree == 0, "should be divisible"
            slice = size // degree
            for i in range(degree):
                print_flatten_array_int(name+"_rank_"+str(i)+".data", tensor[slice * i : slice * (i + 1)])
        elif dim == -1:
            for i in range(degree):
                print_flatten_array_int(name+"_rank_"+str(i)+".data", tensor)
        elif dim == 1:
            shape = list(tensor.size())
            assert len(shape) > 1, "should be at least 2D"
            size = shape[1]
            assert size % degree == 0, "should be divisible"
            slice = size // degree
            for i in range(degree):
                print_flatten_array_int(name+"_rank_"+str(i)+".data", tensor[:, slice * i : slice * (i + 1)])
        elif dim == 2:
            shape = list(tensor.size())
            assert len(shape) > 2, "should be at least 2D"
            size = shape[2]
            assert size % degree == 0, "should be divisible"
            slice = size // degree
            for i in range(degree):
                print_flatten_array_int(name+"_rank_"+str(i)+".data", tensor[:, :, slice * i : slice * (i + 1)])
        else:
            assert False, "TODO"

    return ranked_print_flatten_array_int
