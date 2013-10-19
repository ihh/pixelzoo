#ifndef PROTO_INCLUDED
#define PROTO_INCLUDED

#include "state.h"
#include "vars.h"
#include "stringmap.h"

typedef struct Proto {
  Type type;
  StringMap *varsDescriptorMap;
} Proto;

Proto* newProto (Type type);
void deleteProto (Proto *proto);

void protoAddVarsDescriptor (Proto *proto, const char* varName, unsigned char offset, unsigned char width);

VarsDescriptor* protoGetVarsDescriptor (Proto *proto, const char* varName);
void protoCopyVarsDescriptorsToList (Proto *proto, List* varsDescriptorList);

typedef struct ProtoTable {
  StringMap *proto;
  Type nextFreeType;  /* internal use only */
} ProtoTable;

ProtoTable *newProtoTable();
void deleteProtoTable (ProtoTable* protoTable);

Proto *protoTableAddTypedParticle (ProtoTable *protoTable, const char* particleName, Type particleType);
Proto *protoTableAddParticle (ProtoTable *protoTable, const char* particleName);

Proto *protoTableGetProto (ProtoTable *protoTable, const char* particleName);

#endif /* PROTO_INCLUDED */
