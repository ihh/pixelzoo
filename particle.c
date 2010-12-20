#include <stdlib.h>
#include <string.h>
#include "particle.h"

Particle* newParticle (const char* name, int nRules) {
  Particle* p;
  int c;
  p = SafeMalloc (sizeof (Particle));
  p->type = 0;
  p->name = SafeMalloc ((strlen (name) + 1) * sizeof(char));
  strcpy (p->name, (char*) name);
  for (c = 0; c < NumColorRules; ++c) {
    p->colorRule[c].mask = 0;
    p->colorRule[c].offset = 0;
    p->colorRule[c].multiplier = 0;
    p->colorRule[c].rightShift = 0;
  }
  p->colorRule[0].offset = HSB24White;  /* this ensures that the default ColorRule's generate a white particle */
  p->synchronous = p->shuffle = 0;
  p->nRules = nRules;
  p->rule = SafeCalloc (nRules, sizeof(StochasticRule));
  p->totalRate = p->totalOverloadRate = p->asyncFiringRate = p->firingRate = 0.;
  p->watchers = NULL;
  return p;
}

void deleteParticle (Particle* p) {
  if (p->watchers)
    deleteStringMap (p->watchers);
  SafeFree(p->rule);
  SafeFree(p->name);
  SafeFree(p);
}

PaletteIndex getParticleColor (Particle* particle, State state) {
  int n;
  HSB24 hsb;
  hsb = 0;
  for (n = 0; n < NumColorRules; ++n)
    hsb = hsb + evalColorRule (&particle->colorRule[n], state);
  return ConvertHsb24ToPaletteIndex(hsb);
}
