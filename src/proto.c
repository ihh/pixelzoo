#include "proto.h"

Proto* newProto (Type type) {
  Proto *proto;
  return NULL;
}

void deleteProto (Proto *proto) {
}

void protoAddVarsDescriptor (Proto *proto, const char* varName, unsigned char offset, unsigned char width) {
}

VarsDescriptor* protoGetVarsDescriptor (Proto *proto, const char* varName) {
  return NULL;
}

void protoCopyVarsDescriptorsToList (Proto *proto, List* varsDescriptorList) {
}

ProtoTable *newProtoTable() {
  ProtoTable *protoTable;
  return NULL;
}

void deleteProtoTable (ProtoTable* protoTable) {
}

Proto *protoTableAddTypedParticle (ProtoTable *protoTable, const char* particleName, Type particleType) {
  return NULL;
}

Proto *protoTableAddParticle (ProtoTable *protoTable, const char* particleName) {
  return NULL;
}

Proto *protoTableGetProto (ProtoTable *protoTable, const char* particleName) {
  return NULL;
}

