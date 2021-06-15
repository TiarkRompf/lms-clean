#ifndef SCANNER_HEADER_H
#define SCANNER_HEADER_H

#include <fcntl.h>
#include <stdarg.h>
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

void scan_floats(const char *filename, float *array, int size) {
  FILE *pFile = fopen(filename, "r");
  if (pFile == NULL) {
    char buffer[2000];
    snprintf(buffer, 2000, "Error opening file %s\n", filename);
    perror(buffer);
  }
  for (int i = 0; i < size; i++) {
    if (fscanf(pFile, "%f", array + i) != 1)
      perror("Error scanning binary file for float");
  }
  fclose(pFile);
}

void scan_ints(const char *filename, int *array, int size) {
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

void check_float_array(const char *filename, float *check, int size) {
  float gold[size];
  scan_floats(filename, gold, size);
  if (check_floats(gold, check, size, 0.0005))
    fprintf(stdout, "Checking value with %s passed!\n", filename);
  else
    fprintf(stdout, "Checking value with %s failed!\n", filename);
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

void check_int_array(const char *filename, int *check, int size) {
  int gold[size];
  scan_ints(filename, gold, size);
  if (check_ints(gold, check, size))
    fprintf(stdout, "Checking value with %s passed!\n", filename);
  else
    fprintf(stdout, "Checking value with %s failed!\n", filename);
}

// hacky helper functions that should be handled by codegen instead
void scan_float_rank(const char *filename, int rank, float *array, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  scan_floats(buf, array, size);
}

void scan_float_array(float *array, int size, const char *filenameFormat, int rank) {
  char buf[2000];
  snprintf(buf, 2000, filenameFormat, rank);
  scan_floats(buf, array, size);
}

// void scan_float_array(float *array, int size, const char *filenameFormat, ...) {
//   char buf[2000];
//   va_list vl;
//   va_start( vl, filenameFormat );
//   snprintf(buf, 2000, filenameFormat, vl);
//   scan_floats(buf, array, size);
// }

// hacky helper functions that should be handled by codegen instead
void scan_int_rank(const char *filename, int rank, int *array, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  scan_ints(buf, array, size);
}

void scan_int_array(int* array, int size, const char *filenameFormat, int rank) {
  char buf[2000];
  snprintf(buf, 2000, filenameFormat, rank);
  scan_ints(buf, array, size);
}

// void scan_int_array(int* array, int size, const char *filenameFormat, ...) {
//   char buf[2000];
//   va_list vl;
//   va_start( vl, filenameFormat);
//   snprintf(buf, 2000, filenameFormat, vl);
//   scan_ints(buf, array, size);
// }

void check_float_array_rank(const char *filename, int rank, float *check, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  check_float_array(buf, check, size);
}

void check_int_array_rank(const char *filename, int rank, int *check, int size) {
  // need to build new filename based on rank and some hacky conventions
  char buf[2000];
  snprintf(buf, 2000, "%s_rank_%d.data", filename, rank);
  check_int_array(buf, check, size);
}

void check_float_array_with_file(float *check, int size, const char* filenameFormat, int rank) {
  char buf[2000];
  snprintf(buf, 2000, filenameFormat, rank);
  check_float_array(buf, check, size);
}

// void check_float_array(float *check, int size, const char* filenameFormat, ...) {
//   char buf[2000];
//   va_list vl;
//   va_start( vl, filenameFormat );
//   snprintf(buf, 2000, filenameFormat, vl);
//   check_float_array(buf, check, size);
// }

void check_int_array_with_file(int* check, int size, const char* filenameFormat, int rank) {
  char buf[2000];
  snprintf(buf, 2000, filenameFormat, rank);
  check_int_array(buf, check, size);
}

// void check_int_array(int* check, int size, const char* filenameFormat, ...) {
//   char buf[2000];
//   va_list vl;
//   va_start( vl, filenameFormat );
//   snprintf(buf, 2000, filenameFormat, vl);
//   check_int_array(buf, check, size);
// }

#endif
