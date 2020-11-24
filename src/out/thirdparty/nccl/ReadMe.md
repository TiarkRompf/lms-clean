## Instructions on how to run the checked *.cu files on Purdue ml00 machine
​
### Use Purdue's ml00 cluster
#### 1. ssh to Purdue's ml00 server:
```bash
ssh username@ml00.cs.purdue.edu
```
​
#### 2. After entering the server, load environment for Cuda and NVCC:
```bash
module load slurm
module load cuda
```
​
#### 3. Compile and run using NVCC:
```bash
nvcc -o EXAMPLE.out EXAMPLE.cu
​
# use nccl
nvcc -o EXAMPLE EXAMPLE.cu -l nccl
​
# use mpi and nccl together
nvcc -o EXAMPLE EXAMPLE.cu -l nccl -I/usr/lib/x86_64-linux-gnu/openmpi/include -L/usr/lib/x86_64-linux-gnu/openmpi/lib64 -l mpi
​
# example 1:
nvcc -o multiple_mpi multiple_mpi.cu -l nccl -I/usr/lib/x86_64-linux-gnu/openmpi/include -L/usr/lib/x86_64-linux-gnu/openmpi/lib64 -l mpi
​
# example 2:
nvcc -o mpitest mpitest.cu -l nccl -I/usr/lib/x86_64-linux-gnu/openmpi/include -L/usr/lib/x86_64-linux-gnu/openmpi/lib64 -l mpi
```
​
#### 4. Run the srun command to request a job:
`--gres:gpu` specifies the number of GPUs are required to run the job.
```bash
srun --gres=gpu:2 --cpus-per-task=2 --nice=1000 --partition=ml-all-gpu --time=00:30:00 --pty bash
```
​
​
#### 5. run the code
`--oversubscribe` is required to make sure all available GPUs are used.
`-np` specifies the number of processes should be launched
`EXAMPLE` is the executable
`ARGS` are arguments to `EXAMPLE`
```bash
mpiexec --oversubscribe -np 2 ./EXAMPLE ARGS
```
​
#### 6. Exit after finish running:
```bash
exit
