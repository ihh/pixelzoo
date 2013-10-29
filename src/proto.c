#include <string.h>
#include "proto.h"
#include "chibi.h"

/* Scheme sxml->string procedure, defined in pixelzoo/scheme/zoo.scm */
#define SXML_TO_STRING_PROC "sxml->string"

/* Scheme procedures (built-in) */
#define SET_PROC "set!"

/* Scheme symbol for self */
#define SELF_TYPE "self-type"

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

  protoTable->context = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);

  sexp_load_standard_env(protoTable->context, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(protoTable->context, NULL, stdin, stdout, stderr, 0);

  sexp_load(protoTable->context, sexp_c_string(protoTable->context, sexp_pixelzoo_module_path, -1), NULL);

  protoTable->message = newStringIntMap();
  protoTable->messageText = newStringVector();

  return protoTable;
}

void deleteProtoTable (void* a) {
  ProtoTable *protoTable;
  protoTable = (ProtoTable*) a;
  deleteStringIntMap (protoTable->message);
  deleteStringVector (protoTable->messageText);
  deleteStringMap (protoTable->byName);
  sexp_destroy_context (protoTable->context);
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

sexp protoTableEval (ProtoTable *protoTable, const char* schemeExpression) {
  return sexp_eval_string (protoTable->context, schemeExpression, -1, NULL);
}

const char* protoTableEvalSxml (ProtoTable *protoTable, const char* schemeExpression) {
  sexp ctx;
  char *schemeExprWrapper;
  const char* str;

  str = NULL;
  ctx = protoTable->context;

  /* declare & preserve local variables */
  sexp_gc_var1 (sxml);
  sexp_gc_preserve1 (ctx, sxml);

  /* do some Scheme */
  schemeExprWrapper = SafeMalloc (strlen(SXML_TO_STRING_PROC) + strlen(schemeExpression) + 4);
  sprintf (schemeExprWrapper, "(%s %s)", SXML_TO_STRING_PROC, schemeExpression);
  sxml = sexp_eval_string (protoTable->context, schemeExprWrapper, -1, NULL);
  if (sexp_exceptionp(sxml))
    sexp_print_exception(ctx,sxml,sexp_current_error_port(ctx));
  else if (sexp_stringp(sxml))
    str = StringNew (sexp_string_data (sxml));

  /* release local variables */
  sexp_gc_release4 (ctx);

  return str;
}

void protoTableSetSelfType (ProtoTable *protoTable, const char* selfType) {
  sexp ctx;
  ctx = protoTable->context;

  sexp_eval (ctx, sexp_cons (ctx,
			     sexp_intern(ctx,SET_PROC,-1),
			     sexp_list2 (ctx,
					 sexp_intern(ctx,SELF_TYPE,-1),
					 sexp_c_string(ctx,selfType,-1))), NULL);
}

Message protoTableMessageLookup (ProtoTable *protoTable, const char* message) {
  StringIntMapNode *msgNode;
  Message msg;
  msgNode = StringIntMapFind (protoTable->message, message);
  if (msgNode)
    msg = *(Int64*)msgNode->value;
  else {
    msg = StringVectorSize (protoTable->messageText);
    StringVectorPushBack (protoTable->messageText, message);
    StringIntMapInsert (protoTable->message, message, msg);
  }
  return msg;
}

xmlNode* protoTableExpandSchemeNode (ProtoTable *protoTable, xmlNode *schemeNode, xmlNode *replaceNode, xmlNode *replaceParent) {
  const char *evalResult;
  xmlNode *evalNode;

  evalNode = NULL;
  evalResult = protoTableEvalSxml (protoTable, (const char*) getNodeContent(schemeNode));
  if (evalResult) {
    evalNode = xmlTreeFromString (evalResult);

    /* replace replaceNode with evalNode */
    if (replaceNode) {
      if (replaceParent) {
	if (replaceParent->children == replaceNode)
	  replaceParent->children = evalNode;
	else if (replaceParent->properties == replaceNode)
	  replaceParent->properties = evalNode;
	else if (replaceParent->next == replaceNode)
	  replaceParent->next = evalNode;
	else {
	  for (replaceParent = replaceParent->children; replaceParent; replaceParent = replaceParent->next)
	    if (replaceParent->next == replaceNode)
	      break;
	  Assert (replaceParent && replaceParent->next == replaceNode, "Can't find parent of Scheme node to be replaced");
	  replaceParent->next = evalNode;
	}
      }
      evalNode->next = replaceNode->next;
      replaceNode->next = NULL;
      deleteXmlTree (replaceNode);
    }

    StringDelete ((void*) evalResult);
  }

  return evalNode;
}
