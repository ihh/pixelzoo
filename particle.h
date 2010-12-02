#ifndef PARTICLE_INCLUDED
#define PARTICLE_INCLUDED

#include "rule.h"

/* RGB color */
typedef struct RGB {
  unsigned char r, g, b;
} RGB;

/*
  A Particle consists of:
   a Type,
   a name (C string),
   an RGB color,
   a variable number of StochasticRule's (all of the same Type as the Particle).
 */
typedef struct Particle {
  Type type;
  char* name;
  RGB color;
  int rules;
  StochasticRule* rule;
} Particle;

#endif /* PARTICLE_INCLUDED */
