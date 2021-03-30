#ifndef SCANNER_HEADER_H
#define SCANNER_HEADER_H

#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

long fsize(int fd) {
  struct stat stat;
  int res = fstat(fd, &stat);
  return stat.st_size;
}
int printll(char *s) {
  while (*s != '\n' && *s != ',' && *s != '\t') {
    putchar(*s++);
  }
  return 0;
}
long hash(char *str0, int len) {
  unsigned char *str = (unsigned char *)str0;
  unsigned long hash = 5381;
  int c;

  while ((c = *str++) && len--)
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}

void scan_float(const char *filename, float *array, int size) {
  FILE *pFile = fopen(filename, "r");
  if (pFile == NULL ) {
    perror("Error opening file");
  }
  for (int i = 0; i < size; i++) {
    if (fscanf(pFile, "%f", array + i) != 1)
      perror("Error scanning binary file for float");
  }
  fclose(pFile);
}

void scan_int(const char *filename, int *array, int size) {
  FILE *pFile = fopen(filename, "r");
  if (pFile == NULL) {
    perror("Error opening file");
  }
  for (int i = 0; i < size; i++) {
    if (fscanf(pFile, "%d", array + i) != 1)
      perror("Error scanning binary file for int");
  }
  fclose(pFile);
}

bool check_floats(float *gold, float *check, int size, float eta) {
  bool passed = true;
  for (int i = 0; i < size; i++) {
    if (gold[i] - check[i] < -eta || gold[i] - check[i] > eta) {
      fprintf(stderr, "check failed at index %d: gold: %f check: %f\n", i, gold[i], check[i]);
      passed = false;
    }
  }
  return passed;
}

bool check_float_array(const char *filename, float *gold, float *check, int size) {
  scan_float(filename, gold, size);
  return check_floats(gold, check, size, 0.0005);
}

bool check_float_array(const char *filename, float *check, int size) {
  float gold[size];
  scan_float(filename, gold, size);
  return check_floats(gold, check, size, 0.0005);
}

bool check_ints(int *gold, int *check, int size) {
  bool passed = true;
  for (int i = 0; i < size; i++) {
    if (gold[i] != check[i]) {
      fprintf(stderr, "check failed at index %d: gold %d check %d\n", i, gold[i], check[i]);
      passed = false;
    }
  }
  return passed;
}

bool check_int_array(const char *filename, int *gold, int *check, int size) {
  scan_int(filename, gold, size);
  return check_ints(gold, check, size);
}

bool check_int_array(const char *filename, int *check, int size) {
  int gold[size];
  scan_int(filename, gold, size);
  return check_ints(gold, check, size);
}

// hacky helper functions that should be handled by codegen instead
void scan_float_rank(const char *filename, int rank, float *array, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  scan_float(buf, array, size);
}

// hacky helper functions that should be handled by codegen instead
void scan_int_rank(const char *filename, int rank, int *array, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  scan_int(buf, array, size);
}

void check_float_array_rank(const char *filename, int rank, float *gold, float *check, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  if (check_float_array(buf, gold, check, size))
    fprintf(stdout, "Checking value of %s passed!\n", buf);
  else
    fprintf(stderr, "Checking value of %s failed!\n", buf);
}

void check_int_array_rank(const char *filename, int rank, int *gold, int *check, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  if (check_int_array(buf, gold, check, size))
    fprintf(stdout, "Checking value of %s passed!\n", buf);
  else
    fprintf(stderr, "Checking value of %s failed!\n", buf);
}

#endif
