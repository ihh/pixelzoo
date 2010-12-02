#include <stdlib.h>
#include "util.h"

double random() {
  return (double) rand() / (double) RAND_MAX;
}
