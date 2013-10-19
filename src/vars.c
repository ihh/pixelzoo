#include "vars.h"
#include "util.h"
#include "stringmap.h"

VarsDescriptor *newVarsDescriptor (const char* name, unsigned char offset, unsigned char width) {
  VarsDescriptor* vd;
  vd = SafeMalloc (sizeof (VarsDescriptor));
  vd->name = StringCopy ((void*) name);
  vd->offset = offset;
  vd->width = width;
  return vd;
}

void deleteVarsDescriptor (void *vdVoid) {
  VarsDescriptor *vd;
  vd = (VarsDescriptor*) vdVoid;
  StringDelete (vd->name);
  SafeFree (vd);
}
