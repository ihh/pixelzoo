#ifndef VARS_INCLUDED
#define VARS_INCLUDED

/* Vars field description structure */
typedef struct VarsDescriptor {
  char* name;
  unsigned char offset, width;
} VarsDescriptor;

VarsDescriptor *newVarsDescriptor (const char* name, unsigned char offset, unsigned char width);
void deleteVarsDescriptor (void *vd);

#endif /* VARS_INCLUDED */
