#include <string.h>
#include "proto.h"

Proto* newProto (const char *name, Type type) {
  Proto *proto;
  proto = SafeMalloc (sizeof (Proto));
  proto->type = type;
  proto->name = StringNew (name);
  proto->varsDescriptorMap = newStringMap (copyVarsDescriptor, deleteVarsDescriptor, NullPrintFunction);
  proto->varName = newStringVector();
  proto->nextOffset = 0;
  return proto;
}

void deleteProto (void *a) {
  Proto *proto;
  proto = (Proto*) a;
  deleteStringMap (proto->varsDescriptorMap);
  deleteStringVector (proto->varName);
  StringDelete (proto->name);
  SafeFree (proto);
}

void* copyProto (void *a) {
  Proto *proto, *proto_orig;
  proto_orig = (Proto*) a;
  proto = SafeMalloc (sizeof (Proto));
  proto->type = proto_orig->type;
  proto->name = StringCopy (proto_orig->name);
  proto->varsDescriptorMap = (StringMap*) RBTreeDeepCopy (proto_orig->varsDescriptorMap);
  proto->varName = (StringVector*) VectorDeepCopy (proto_orig->varName);
  return proto;
}

VarsDescriptor* protoAddVar (Proto *proto, const char* varName, unsigned char width) {
  VarsDescriptor *varsDescriptor;
  varsDescriptor = newVarsDescriptor (varName, proto->nextOffset, width);
  (void) StringMapInsert (proto->varsDescriptorMap, varName, varsDescriptor);
  (void) StringVectorPushBack (proto->varName, varName);
  proto->nextOffset += width;
  Assert (proto->nextOffset <= BitsPerVars, "Too many Vars");
  return varsDescriptor;
}

VarsDescriptor* protoGetVarsDescriptor (Proto *proto, const char* varName) {
  StringMapNode* node;
  node = StringMapFind (proto->varsDescriptorMap, varName);
  return node ? (VarsDescriptor*) node->value : (VarsDescriptor*) NULL;
}

void protoCopyVarsDescriptorsToList (Proto *proto, List* varsDescriptorList) {
  int i;
  for (i = 0; i < StringVectorSize(proto->varName); ++i)
    ListAppend (varsDescriptorList, copyVarsDescriptor (StringMapFind (proto->varsDescriptorMap, StringVectorGet (proto->varName, i))->value));
}

ProtoTable *newProtoTable() {
  ProtoTable *protoTable;
  protoTable = SafeMalloc (sizeof (ProtoTable));
  protoTable->byName = newStringMap (copyProto, deleteProto, NullPrintFunction);
  protoTable->byType = SafeCalloc (NumTypes, sizeof (Proto*));
  protoTable->nextFreeType = 0;
  return protoTable;
}

void deleteProtoTable (void* a) {
  ProtoTable *protoTable;
  protoTable = (ProtoTable*) a;
  deleteStringMap (protoTable->byName);
  SafeFree (protoTable->byType);
  SafeFree (protoTable);
}

Proto *protoTableAddTypedParticle (ProtoTable *protoTable, const char* particleName, Type particleType) {
  Proto *proto;
  if (protoTable->byType[particleType])
    Abort ("Attempt to redefine Particle Type");
  proto = newProto (particleName, particleType);
  protoTable->byType[particleType] = proto;
  StringMapInsert (protoTable->byName, particleName, proto);
  return proto;
}

Proto *protoTableAddParticle (ProtoTable *protoTable, const char* particleName) {
  Proto *proto;
  while (protoTable->nextFreeType < MaxType && protoTable->byType[protoTable->nextFreeType] != NULL)
    ++protoTable->nextFreeType;
  if (protoTable->byType[protoTable->nextFreeType] == NULL) {
    proto = protoTableAddTypedParticle (protoTable, particleName, protoTable->nextFreeType);
    if (protoTable->nextFreeType < MaxType)
      ++protoTable->nextFreeType;
    return proto;
  }
  Abort ("Too many Particles");
  return NULL;
}

Proto *protoTableGetProto (ProtoTable *protoTable, const char* particleName) {
  StringMapNode *node;
  node = StringMapFind (protoTable->byName, particleName);
  return node ? (Proto*) node->value : (Proto*) NULL;
}
