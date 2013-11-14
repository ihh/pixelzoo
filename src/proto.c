#include <string.h>
#include "proto.h"
#include "chibi.h"

/* Scheme sxml->string procedure, defined in pixelzoo/scheme/zoo.scm */
#define SXML_TO_STRING_PROC "sxml->string"

/* Scheme procedures (built-in) */
#define SET_PROC "set!"

/* Scheme symbols */
#define SELF_TYPE     "self-type"
#define CONTEST_TYPE  "contest-type"
#define CONTEST_VAR   "contest-var"
#define INCUMBENT_ID  "incumbent-id"
#define CHALLENGER_ID "challenger-id"

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
  sexp_gc_var2 (name, env);

  protoTable = SafeMalloc (sizeof (ProtoTable));
  protoTable->byName = newStringMap (copyProto, deleteProto, NullPrintFunction);
  protoTable->byType = SafeCalloc (NumTypes, sizeof (Proto*));
  protoTable->nextFreeType = 0;

  /* initialize Scheme */
  protoTable->context = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_gc_preserve2 (protoTable->context, name, env);

  sexp_load_standard_env(protoTable->context, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(protoTable->context, NULL, stdin, stdout, stderr, 0);

  name = sexp_c_string(protoTable->context, sexp_pixelzoo_module_path, -1);
  sexp_load(protoTable->context, name, NULL);

  env = sexp_context_env(protoTable->context);
  sexp_init_lib_board (protoTable->context, env);

  sexp_gc_release2 (protoTable->context);
  /* end of Scheme intialization */

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

void protoTableEval (ProtoTable *protoTable, const char* schemeExpression) {
  sexp_eval_string (protoTable->context, schemeExpression, -1, NULL);
}

const char* protoTableEvalSxml (ProtoTable *protoTable, const char* schemeExpression) {
  sexp ctx;
  char *schemeExprWrapper;
  const char* str;

  /* declare local variables */
  sexp_gc_var1 (sxml);

  str = NULL;
  ctx = protoTable->context;

  /* preserve local variables */
  sexp_gc_preserve1 (ctx, sxml);

  /* do some Scheme */
  schemeExprWrapper = SafeMalloc (strlen(SXML_TO_STRING_PROC) + strlen(schemeExpression) + 4);
  sprintf (schemeExprWrapper, "(%s %s)", SXML_TO_STRING_PROC, schemeExpression);

  sxml = sexp_eval_string (protoTable->context, schemeExprWrapper, -1, NULL);
  if (sexp_exceptionp(sxml))
    sexp_print_exception(ctx,sxml,sexp_current_error_port(ctx));
  else if (sexp_stringp(sxml))
    str = StringNew (sexp_string_data (sxml));

  /*
    Warn("%s evaluated to '%s'",schemeExprWrapper,str);
  */

  SafeFree (schemeExprWrapper);

  /* release local variables */
  sexp_gc_release1 (ctx);

  return str;
}

void protoTableSetString (ProtoTable *protoTable, const char* symbol, const char* value) {
  char* str;
  sexp ctx;
  sexp_gc_var2 (tmp, f);

  ctx = protoTable->context;
  sexp_gc_preserve2 (ctx, tmp, f);

  tmp = sexp_c_string(ctx,value,-1);
  f = sexp_cons(ctx,tmp,SEXP_NULL);

  tmp = sexp_intern(ctx,symbol,-1);
  f = sexp_cons(ctx,tmp,f);

  tmp = sexp_intern(ctx,SET_PROC,-1);
  f = sexp_cons(ctx,tmp,f);

  sexp_eval (ctx, f, NULL);

  sexp_gc_release2 (ctx);

  /* test */
  str = sexp_string_data (sexp_eval_string (protoTable->context, symbol, -1, NULL));
  Assert (strcmp(str,value)==0, "%s evaluates to '%s', should be '%s'", symbol, str, value);
}

void protoTableSetInt (ProtoTable *protoTable, const char* symbol, int value) {
  int n;
  sexp ctx;
  sexp_gc_var2 (tmp, f);

  ctx = protoTable->context;
  sexp_gc_preserve2 (ctx, tmp, f);

  tmp = sexp_make_integer(ctx,value);
  f = sexp_cons(ctx,tmp,SEXP_NULL);

  tmp = sexp_intern(ctx,symbol,-1);
  f = sexp_cons(ctx,tmp,f);

  tmp = sexp_intern(ctx,SET_PROC,-1);
  f = sexp_cons(ctx,tmp,f);

  sexp_eval (ctx, f, NULL);

  sexp_gc_release2 (ctx);

  /* test */
  n = sexp_sint_value (sexp_eval_string (protoTable->context, symbol, -1, NULL));
  Assert (n == value, "%s evaluates to %d, should be %d", symbol, n, value);
}

void protoTableSetSelfType (ProtoTable *protoTable, const char* selfType) {
  protoTableSetString (protoTable, SELF_TYPE, selfType);
}

void protoTableSetContestInfo (ProtoTable *protoTable, const char* winType, const char* winVar, int incumbent, int challenger) {
  protoTableSetString (protoTable, CONTEST_TYPE, winType);
  protoTableSetString (protoTable, CONTEST_VAR, winVar);
  protoTableSetInt (protoTable, INCUMBENT_ID, incumbent);
  protoTableSetInt (protoTable, CHALLENGER_ID, challenger);
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
      Assert (strcmp((const char*)evalNode->name,(const char*)replaceNode->name) == 0, "Expanded Scheme node <%s> does not match replacement node <%s>",(const char*)evalNode->name,(const char*)replaceNode->name);
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
