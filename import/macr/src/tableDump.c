#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

static void usage();

typedef enum { CHARACTER, INTEGER, REAL } ColumnType;
typedef struct Column {
  size_t start;
  size_t end;
  ColumnType type;
  const char* name;
  const char* na;
} Column;

// specified as in the file to avoid confusion, not 0-based indexing
static Column columns[] = {
  { 1, 2, INTEGER, "record_type_id", "NA" },
  { 3, 7, INTEGER, "bcs_jurisdiction", "NA" },
  { 8, 11, CHARACTER, "ncic_jurisdiction", NULL },
  // { 12, 13, CHARACTER, "skip_1", NULL },
  { 14, 17, INTEGER, "arrest_year", "NA" },
  { 18, 19, INTEGER, "arrest_month", "NA" },
  { 20, 21, INTEGER, "arrest_day", "NA" },
  { 22, 22, CHARACTER, "summary_offense_level", "NA" },
  { 23, 23, CHARACTER, "offense_level", NULL },
  { 24, 26, INTEGER, "bcs_offense_code", "NA" },
  { 27, 28, INTEGER, "bcs_summary_offense_code", "NA" },
  // { 29, 29, CHARACTER, "skip_2", NULL },
  { 30, 32, CHARACTER, "fbi_offense_code", NULL },
  // { 33, 39, INTEGER, "id", "NA" },
  // { 40, 69, CHARACTER, "name", NULL },
  { 70, 73, INTEGER, "birth_year", "NA" },
  { 74, 75, INTEGER, "birth_month", "NA" },
  { 76, 77, INTEGER, "birth_day", "NA" },
  { 78, 80, INTEGER, "age", "NA" },
  { 81, 81, CHARACTER, "race_or_ethnicity", "NA" },
  { 82, 82, INTEGER, "gender", "NA" },
  { 83, 83, INTEGER, "status_type", "NA" },
  { 84, 84, INTEGER, "disposition", "NA" },
  { 0, 0, CHARACTER, NULL }
};

static void printColumn(FILE* outfile, const char* colString, const Column* col)
{
  errno = 0;
  switch (col->type) {
    case CHARACTER:
    {
      const char* start = colString;
      while (*start != '\0' && (*start == ' ' || *start == '\t')) ++start;
      if (*start != '\0') {
        const char* end = start + 1;
        while (*end != '\0') ++end;
        --end;
        while (end > start && (*end == ' ' || *end == '\t')) --end;
        ++end;
        fprintf(outfile, "%.*s", (int) (end - start), start);
      }
      // fprintf(outfile, "%s", colString);
    }
    break;
    case INTEGER:
    {
      if (colString[0] == ' ') {
        fprintf(outfile, "%s", col->na);
      } else {
        long i = strtol(colString, NULL, 10);
        if (errno == EINVAL || errno == ERANGE)
          fprintf(outfile, "%s", col->na);
        else
          fprintf(outfile, "%ld", i);
      }
    }
    break;
    case REAL:
    {
      if (colString[0] == ' ') {
        fprintf(outfile, "%s", col->na);
      } else {
        double d = strtod(colString, NULL);
        if (errno == EINVAL || errno == ERANGE)
          fprintf(outfile, "%s", col->na);
        else
          fprintf(outfile, "%f", d);
      }
    }
    break;
  }
}

int main(int argc, char** argv)
{
  if (argc != 3) usage();
  
  const char* infileName  = argv[1];
  const char* outfileName = argv[2];
  
  int infile = open(infileName, O_RDONLY);
  if (infile == -1) {
    fprintf(stderr, "error opening infile: %s\n", strerror(errno));
    exit(errno);
  }
  
  /*
  FILE* inFileDesc = fopen(inFileName, "r");
  size_t rowLength = 0;
  int c;
  while (TRUE) {
    c = fgetc(inFileDesc);
    if (c == EOF) {
      fprintf(stderr, "infile contains no line breaks\n");
      fclose(inFileDesc);
      exit(ERANGE);
    }
    if (c == '\n') break;
    ++rowLength;
  }
  fclose(inFileDesc); */
  
  size_t inLength = (size_t) lseek(infile, 0, SEEK_END);
  lseek(infile, 0, SEEK_SET);
      
  void* inPtr = mmap(NULL, inLength, PROT_READ, MAP_PRIVATE, infile, 0);
  if (inPtr == MAP_FAILED) {
    fprintf(stderr, "error mapping infile: %s\n", strerror(errno));
    close(infile);
    exit(errno);
  }
  
  const char* start = (const char*) inPtr;
  const char* end   = start + inLength;
  
  const char* c;
  for (c = start; c < end && *c != '\n'; ++c) { }
  if (c == end) {
    fprintf(stderr, "infile contains no line breaks\n");
    munmap(inPtr, inLength);
    close(infile);
    exit(ERANGE);
  }
  size_t rowLength = c - start + 1;
    
  if (inLength % rowLength != 0) {
    fprintf(stderr, "length of infile not a multiple of row length (%lu / %lu)\n", inLength, rowLength);
    munmap(inPtr, inLength);
    close(infile);
    exit(EINVAL);
  }
  
  FILE* outfile = fopen(outfileName, "w");
  if (outfile == NULL) {
    fprintf(stderr, "error opening outfile: %s\n", strerror(errno));
    munmap(inPtr, inLength);
    close(infile);
    exit(errno);
  }
  
  size_t maxColumnWidth = 0;
  for (Column* column = columns; column->name != NULL; ++column) {
    if (column->end - column->start + 1 > maxColumnWidth)
      maxColumnWidth = column->end - column->start + 1;
  }
  
  char* buffer = (char*) malloc(maxColumnWidth + 1);
  if (buffer == NULL) {
    fprintf(stderr, "unable to create string buffer\n");
    fclose(outfile);
    munmap(inPtr, inLength);
    close(infile);
    exit(ENOMEM);
  }
  
  Column* column = columns;
  fprintf(outfile, "%s", column->name);
  ++column;
  for (; column->name != NULL; ++column) fprintf(outfile, "|%s", column->name);
  fprintf(outfile, "\n");
  
  size_t numRows = inLength / rowLength;
  for (size_t rowNum = 0; rowNum < numRows; ++rowNum) {
    column = columns;
    
    size_t colLength = column->end - column->start + 1;
    memcpy(buffer, start + column->start - 1, colLength);
    buffer[column->end - column->start + 1] = '\0';
    
    printColumn(outfile, buffer, column);
    ++column;
    
    for (; column->name != NULL; ++column) {
      colLength = column->end - column->start + 1;
      memcpy(buffer, start + column->start - 1, colLength);
      buffer[column->end - column->start + 1] = '\0';
      
      fprintf(outfile, "|");
      printColumn(outfile, buffer, column);
    }
    fprintf(outfile, "\n");
    
    start += rowLength;
  }
  
  free(buffer);
  fclose(outfile);
  munmap(inPtr, inLength);
  close(infile);
  
  return 0;
}

static void usage()
{
  fprintf(stderr, "usage: tableDump infile outfile\n");
  exit(-1);
}
