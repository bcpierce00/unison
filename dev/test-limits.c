/*
 * test-limit.c: Test memory size limits.
 * Allocate memory until failure.
 * Print amount allocated.
 *
 * https://pubs.opengroup.org/onlinepubs/9799919799/utilities/ulimit.html
 * https://pubs.opengroup.org/onlinepubs/9799919799/functions/setrlimit.html
 */

#include <stdio.h>
#include <stdlib.h>

#define BUFSIZK	4

int
main(int argc, char **argv)
{
  int i;
  int *buf;

  /*
   * Limit to 4 GB, to avoid problems if memory limits are not
   * working as expected.
   */
  for (i = 0; i < (4 * 1024 * 1024) / BUFSIZK; i++) {
    /* Allocate, write to force a page, and discard. */
    buf = malloc(BUFSIZK * 1024);
    if (buf == NULL)
      break;
    buf[0] = 0;
    buf = NULL;
  }

  /* Print in kB. */
  printf("%d\n", i * BUFSIZK);
}
