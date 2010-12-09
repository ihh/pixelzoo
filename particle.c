#include <stdlib.h>
#include <string.h>
#include "particle.h"

Particle* newParticle (const char* name, int nRules) {
  Particle* p;
  p = SafeMalloc (sizeof (Particle));
  p->type = 0;
  p->name = calloc (strlen (name) + 1, sizeof(char));
  strcpy ((char*) name, p->name);
  p->color.r = p->color.g = p->color.b = 0;
  p->nRules = nRules;
  p->rule = calloc (nRules, sizeof(StochasticRule));
  p->totalRate = p->normalizedRate = 0.;
  return p;
}

void deleteParticle (Particle* p) {
  SafeFree(p->rule);
  SafeFree(p->name);
  SafeFree(p);
}
