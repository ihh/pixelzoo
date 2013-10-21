#ifndef PROTO_INCLUDED
#define PROTO_INCLUDED

#include "state.h"
#include "vars.h"
#include "stringmap.h"

#include "chibi/eval.h"
#include "chibi/sexp.h"

typedef struct Proto {
  char *name;
  Type type;
  StringMap *varsDescriptorMap;
  StringVector *varName;
  unsigned char nextOffset;  /* internal use only */
} Proto;

Proto* newProto (const char *name, Type type);
void deleteProto (void *proto);
void* copyProto (void *a);

VarsDescriptor* protoAddVar (Proto *proto, const char* varName, unsigned char width);

VarsDescriptor* protoGetVarsDescriptor (Proto *proto, const char* varName);
void protoCopyVarsDescriptorsToList (Proto *proto, List* varsDescriptorList);

typedef struct ProtoTable {
  StringMap *byName;
  Proto **byType;
  Type nextFreeType;  /* internal use only */
  sexp context;  /* Chibi Scheme context */
} ProtoTable;

ProtoTable *newProtoTable();
void deleteProtoTable (void* protoTable);

Proto *protoTableAddTypedParticle (ProtoTable *protoTable, const char* particleName, Type particleType);
Proto *protoTableAddParticle (ProtoTable *protoTable, const char* particleName);

Proto *protoTableGetProto (ProtoTable *protoTable, const char* particleName);

sexp protoTableMakeContext (ProtoTable *protoTable);
#define protoTableDestroyContext(ctx) sexp_destroy_context(ctx)

#endif /* PROTO_INCLUDED */
