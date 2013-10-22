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

sexp protoTableEval (ProtoTable *protoTable, const char* schemeExpression);  /* use for top-level <grammar> definitions */
void protoTableSetSelfType (ProtoTable *protoTable, const char* selfType);
const char* protoTableEvalSxml (ProtoTable *protoTable, const char* schemeExpression);  /* use for <rule> evaluations. Auto-converts result from SXML to XML. Caller must free returned string */

#endif /* PROTO_INCLUDED */
