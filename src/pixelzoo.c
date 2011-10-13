#include <string.h>

#include "pixelzoo.h"
#include "xmlgame.h"
#include "xmlmove.h"

#define MAX_PROPORTION_TIME_EVOLVING .9  /* so that gameLoop doesn't eat 100% of the time between updates */

pzGame pzNewGameFromXmlString(const char*gameString) {
  return (pzGame) newGameFromXmlString(gameString);
}

pzGame pzRestoreBoardFromXmlString(const char*gameString,const char*boardString) {
  return (pzGame) newGameFromXmlStringWithSeparateBoard (gameString, boardString);
}

void pzDeleteGame(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  deleteGame(game);
}

void pzStartGame(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  gameStart(game);
}

int pzGameRunning(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  return gameRunning(game);
}

void pzUpdateGame(pzGame pzg,int callsPerSecond) {
  double targetTicks, actualTicks, elapsedTime;
  int64_Microticks actualMicroticks;
  int cellUpdates;
  Game* game;
  game = (Game*) pzg;
  targetTicks = game->ticksPerSecond / (double) callsPerSecond;
  gameLoop (game, targetTicks, MAX_PROPORTION_TIME_EVOLVING, &actualMicroticks, &actualTicks, &cellUpdates, &elapsedTime);
}

int pzGetBoardSize(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  return game->board->size;
}

int pzGetCellRgb(pzGame pzg,int x,int y) {
  PaletteIndex cellColorIndex;
  RGB *cellRgb;
  Game* game;
  game = (Game*) pzg;
  cellColorIndex = readBoardColor(game->board, x, y);
  cellRgb = &game->board->palette.rgb[cellColorIndex];
  return PackRgbTo24Bit(*cellRgb);
}

const char* pzGetCellName(pzGame pzg,int x,int y) {
  State examState;
  Particle *particle;
  char *text;
  Game* game;
  game = (Game*) pzg;
  examState = readBoardState (game->board, x, y);
  particle = game->board->byType[StateType(examState)];
  text = particle ? particle->name : NULL;
  return text;
}

int pzGetCellNameRgb(pzGame pzg,int x,int y) {
  State examState;
  Particle *particle;
  PaletteIndex examColorIndex;
  RGB *cellNameRgb;
  Game* game;
  game = (Game*) pzg;
  examState = readBoardState (game->board, x, y);
  particle = game->board->byType[StateType(examState)];
  examColorIndex = particle ? getParticleColor (particle, examState) : PaletteWhite;
  cellNameRgb = &game->board->palette.rgb[examColorIndex];
  return PackRgbTo24Bit(*cellNameRgb);
}

int pzGetNumberOfTools(pzGame pzg) {
  ListNode *toolNode;
  Tool *tool;
  int nTools;
  Game* game;
  game = (Game*) pzg;
  nTools = 0;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
    tool = toolNode->value;
    if (!tool->hidden)
      ++nTools;
  }
  return nTools;
}

pzTool getToolByNumber(pzGame pzg,int toolNum) {
  ListNode *toolNode;
  Tool *tool;
  Game* game;
  game = (Game*) pzg;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
    tool = toolNode->value;
    if (!tool->hidden)
      if (--toolNum < 0)
	return (pzTool) tool;
  }
  return (pzTool) NULL;
}

const char* pzGetToolName(pzTool pzt) {
  const char *toolName;
  Tool* tool;
  tool = (Tool*) pzt;
  toolName = tool ? tool->name : NULL;
  return toolName;
}

int getSelectedToolNumber(pzGame pzg) {
  ListNode *toolNode;
  Tool *tool;
  int selectedToolNum, toolNum;
  Game* game;
  game = (Game*) pzg;
  toolNum = 0;
  selectedToolNum = -1;
  if (game->selectedTool)
    for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
      tool = toolNode->value;
      if (!tool->hidden) {
	if (tool == game->selectedTool) {
	  selectedToolNum = toolNum;
	  break;
	}
	++toolNum;
      }
    }
  return selectedToolNum;
}

int pzGetToolRgb(pzGame pzg,pzTool pzt) {
  State toolState;
  PaletteIndex toolColorIndex;
  RGB *toolRgb;
  Game* game;
  Tool* tool;
  tool = (Tool*) pzt;
  game = (Game*) pzg;
  toolColorIndex = PaletteBlack;
  if (tool) {
    toolState = tool->defaultBrushState;
    toolColorIndex = getParticleColor (game->board->byType[StateType(toolState)], toolState);
  }
  toolRgb = &game->board->palette.rgb[toolColorIndex];
  return PackRgbTo24Bit(*toolRgb);
}

double pzGetToolReserveLevel(pzTool pzt) {
  double toolReserveLevel;
  Tool* tool;
  tool = (Tool*) pzt;
  toolReserveLevel = tool ? (tool->reserve / tool->maxReserve) : 0.;
  return toolReserveLevel;
}

void pzSelectTool(pzGame pzg,int toolNum) {
  Game* game;
  game = (Game*) pzg;
  game->selectedTool = getToolByNumber (game, toolNum);
}

void pzUnselectTool(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  game->selectedTool = NULL;
}

void pzTouchCell(pzGame pzg,int x,int y) {
  Game* game;
  game = (Game*) pzg;
  game->toolPos.x = x;
  game->toolPos.y = y;
  if (!game->toolActive)
    game->lastToolPos = game->toolPos;
  game->toolActive = 1;
}

void pzUntouchCell(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  game->toolActive = 0;
}

int pzGetNumberOfConsoleLines(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  return ConsoleLines;
}

const char* pzGetConsoleText(pzGame pzg,int lineNum) {
  int ci;
  Game* game;
  game = (Game*) pzg;
  ci = (lineNum + game->consoleLastLineIndex) % ConsoleLines;
  return game->consoleText[ci];
}

int pzGetNumberOfBalloons(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  return VectorSize (game->board->balloon);
}

pzBalloon pzGetBalloonByNumber(pzGame pzg,int balloonNum) {
  Game* game;
  game = (Game*) pzg;
  return (pzBalloon) VectorGet (game->board->balloon, balloonNum);
}

const char* pzGetBalloonText(pzBalloon pzb) { return ((Balloon*)pzb)->text; }
double pzGetBalloonXpos(pzBalloon pzb) { return ((Balloon*)pzb)->x; }
double pzGetBalloonYpos(pzBalloon pzb) { return ((Balloon*)pzb)->y; }
double pzGetBalloonCharSize(pzBalloon pzb) { return ((Balloon*)pzb)->size; }
double pzGetBalloonCharSpacing(pzBalloon pzb) { return ((Balloon*)pzb)->z; }
int pzGetBalloonTextRgb(pzGame pzg,pzBalloon pzb) {
  RGB *textRgb;
  Game* game;
  game = (Game*) pzg;
  textRgb = &game->board->palette.rgb[((Balloon*)pzb)->color];
  return PackRgbTo24Bit(*textRgb);
}
double pzGetBalloonOpacity(pzBalloon pzb) { return ((Balloon*)pzb)->opacity; }

const char* pzGetMoveAsXmlString(pzGame pzg) {
  xmlBufferPtr buf;
  xmlTextWriterPtr writer;
  const char* str;
  Game* game;
  game = (Game*) pzg;
  str = NULL;
  buf = xmlBufferCreate();
  if (buf) {
    writer = xmlNewTextWriterMemory(buf, 0);
    if (xmlTextWriterStartDocument (writer, NULL, NULL, NULL) >= 0) {
      writeMoveList (game->board->moveLog, writer, (xmlChar*) XMLZOO_LOG);
      str = SafeCalloc (buf->use + 1, sizeof(char));
      strcpy ((char*) str, (char*) buf->content);
    }
    xmlBufferFree (buf);
  }
  return str;
}

const char* pzSaveBoardAsXmlString(pzGame pzg) {
  xmlBufferPtr buf;
  xmlTextWriterPtr writer;
  const char* str;
  Game* game;
  game = (Game*) pzg;
  str = NULL;
  buf = xmlBufferCreate();
  if (buf) {
    writer = xmlNewTextWriterMemory(buf, 0);
    if (xmlTextWriterStartDocument (writer, NULL, NULL, NULL) >= 0) {
      writeBoard (game->board, writer, 1);
      str = SafeCalloc (buf->use + 1, sizeof(char));
      strcpy ((char*) str, (char*) buf->content);
    }
    xmlBufferFree (buf);
  }
  return str;
}
