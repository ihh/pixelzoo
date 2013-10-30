#include "xmlmove.h"
#include "xmlutil.h"

Move* newMoveFromXmlNode (xmlNode *moveNode, ProtoTable *protoTable) {
  Assert (moveNode != NULL, "newMoveFromXmlNode: null move node");
  return newMove (CHILDINT(moveNode,TIME),
		  OPTCHILDINT(moveNode,UPDATE,-1),
		  CHILDINT(moveNode,X),
		  CHILDINT(moveNode,Y),
		  CHILDINT(moveNode,Z),
		  getStateFromNode(moveNode,protoTable,0));
}

MoveList* newMoveListFromXmlNode (xmlNode *moveListNode, ProtoTable *protoTable) {
  MoveList *moveList;
  MoveListNode *lastMoveListNode;
  xmlNode *node;

  Assert (moveListNode != NULL, "newMoveListFromXmlNode: null moveList node");

  moveList = newMoveList();

  for (node = moveListNode->children; node; node = node->next)
    if (MATCHES(node,MOVE)) {
      lastMoveListNode = moveList->tail;
      ListAppend (moveList, newMoveFromXmlNode (node, protoTable));
      Assert (lastMoveListNode == NULL || ((Move*)moveList->tail->value)->t >= ((Move*)lastMoveListNode->value)->t, "newMoveListFromXmlNode: moves not sorted by time");
    }

  return moveList;
}

void writeMove (Move* move, xmlTextWriterPtr writer) {
  xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_MOVE);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_TIME, "%lld", move->t);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_UPDATE, "%lld", move->u);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_X, "%d", move->x);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_Y, "%d", move->y);
  xmlTextWriterWriteFormatElement (writer, (xmlChar*) XMLZOO_HEXSTATE, "%llx", move->state);
  xmlTextWriterEndElement (writer);
}

void writeMoveList (MoveList* moveList, xmlTextWriterPtr writer, const xmlChar* name) {
  MoveListNode *moveListNode;
  if (moveList) {
    xmlTextWriterStartElement (writer, name);
    for (moveListNode = moveList->head; moveListNode; moveListNode = moveListNode->next)
      writeMove ((Move*) moveListNode->value, writer);
    xmlTextWriterFullEndElement (writer);
  }
}
