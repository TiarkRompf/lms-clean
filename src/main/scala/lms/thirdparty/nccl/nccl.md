This folder tries to support NCCL (https://github.com/NVIDIA/nccl) as thirdparty library.

Cited from the NCCL github page:
NCCL (pronounced "Nickel") is a stand-alone library of standard collective communication routines for GPUs,
implementing all-reduce, all-gather, reduce, broadcast, and reduce-scatter.
It has been optimized to achieve high bandwidth on platforms using PCIe, NVLink, NVswitch,
as well as networking using InfiniBand Verbs or TCP/IP sockets.
NCCL supports an arbitrary number of GPUs installed in a single node or across multiple nodes,
and can be used in either single- or multi-process (e.g., MPI) applications.

This folder will mostly focus on the using examples, APIs, and LMS constructs that
can allow easy usage of NCCL.
