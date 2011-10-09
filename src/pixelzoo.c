#include "pixelzoo.h"
#include "xmlmove.h"

#define MAX_PROPORTION_TIME_EVOLVING .9  /* so that gameLoop doesn't eat 100% of the time between updates */

Game* pzNewGameFromXmlString(const char*gameString) {
  return newGameFromXmlString(gameString);
}

Game* pzRestoreBoardFromXmlString(const char*gameString,const char*boardString) {
  return newGameFromXmlStringWithSeparateBoard (gameString, boardString);
}

void pzDeleteGame(Game*game) {
  deleteGame(game);
}

void pzStartGame(Game*game) {
  gameStart(game);
}

int pzGameRunning(Game*game) {
  return gameRunning(game);
}

void pzUpdateGame(Game*game,int callsPerSecond) {
  double targetTicks, actualTicks, elapsedTime;
  int64_Microticks actualMicroticks;
  int cellUpdates;
  targetTicks = game->ticksPerSecond / (double) callsPerSecond;
  gameLoop (game, targetTicks, MAX_PROPORTION_TIME_EVOLVING, &actualMicroticks, &actualTicks, &cellUpdates, &elapsedTime);
}

int pzGetBoardSize(Game*game) {
  return game->board->size;
}

int pzGetCellRgb(Game*game,int x,int y) {
  PaletteIndex cellColorIndex;
  RGB *cellRgb;
  cellColorIndex = readBoardColor(game->board, x, y);
  cellRgb = &game->board->palette.rgb[cellColorIndex];
  return PackRgbTo24Bit(*cellRgb);
}

const char* pzGetCellName(Game*game,int x,int y) {
  State examState;
  Particle *particle;
  char *text;
  examState = readBoardState (game->board, x, y);
  particle = game->board->byType[StateType(examState)];
  text = particle ? particle->name : NULL;
  return text;
}

int pzGetCellNameRgb(Game*game,int x,int y) {
  State examState;
  Particle *particle;
  PaletteIndex examColorIndex;
  RGB *cellNameRgb;
  examState = readBoardState (game->board, x, y);
  particle = game->board->byType[StateType(examState)];
  examColorIndex = particle ? getParticleColor (particle, examState) : PaletteWhite;
  cellNameRgb = &game->board->palette.rgb[examColorIndex];
  return PackRgbTo24Bit(*cellNameRgb);
}

int pzGetNumberOfTools(Game*game) {
  ListNode *toolNode;
  Tool *tool;
  int nTools;
  nTools = 0;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
    tool = toolNode->value;
    if (!tool->hidden)
      ++nTools;
  }
  return nTools;
}

Tool* getToolByNumber(Game*game,int toolNum) {
  ListNode *toolNode;
  Tool *tool;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
    tool = toolNode->value;
    if (!tool->hidden)
      if (--toolNum < 0)
	return tool;
  }
  return NULL;
}

const char* pzGetToolName(Tool*tool) {
  const char *toolName;
  toolName = tool ? tool->name : NULL;
  return toolName;
}

int getSelectedToolNumber(Game*game) {
  ListNode *toolNode;
  Tool *tool;
  int selectedToolNum, toolNum;
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

int pzGetToolRgb(Game*game,Tool*tool) {
  State toolState;
  PaletteIndex toolColorIndex;
  RGB *toolRgb;
  toolColorIndex = PaletteBlack;
  if (tool) {
    toolState = tool->defaultBrushState;
    toolColorIndex = getParticleColor (game->board->byType[StateType(toolState)], toolState);
  }
  toolRgb = &game->board->palette.rgb[toolColorIndex];
  return PackRgbTo24Bit(*toolRgb);
}

double pzGetToolReserveLevel(Tool*tool) {
  double toolReserveLevel;
  toolReserveLevel = tool ? (tool->reserve / tool->maxReserve) : 0.;
  return toolReserveLevel;
}

void pzSelectTool(Game*game,int toolNum) {
  game->selectedTool = getToolByNumber (game, toolNum);
}

void pzUnselectTool(Game*game) {
  game->selectedTool = NULL;
}

void pzTouchCell(Game*game,int x,int y) {
  game->toolPos.x = x;
  game->toolPos.y = y;
  if (!game->toolActive)
    game->lastToolPos = game->toolPos;
  game->toolActive = 1;
}

void pzUntouchCell(Game*game) {
  game->toolActive = 0;
}

int pzGetNumberOfConsoleLines(Game*game) {
  return ConsoleLines;
}

const char* pzGetConsoleText(Game*game,int lineNum) {
  int ci;
  ci = (lineNum + game->consoleLastLineIndex) % ConsoleLines;
  return game->consoleText[ci];
}

int pzGetNumberOfBalloons(Game*game) {
  return VectorSize (game->board->balloon);
}

Balloon* pzGetBalloonByNumber(Game*game,int balloonNum) {
  return (Balloon*) VectorGet (game->board->balloon, balloonNum);
}

const char* pzGetBalloonText(Balloon*balloon) { return balloon->text; }
double pzGetBalloonXpos(Balloon*balloon) { return balloon->x; }
double pzGetBalloonYpos(Balloon*balloon) { return balloon->y; }
double pzGetBalloonCharSize(Balloon*balloon) { return balloon->size; }
double pzGetBalloonCharSpacing(Balloon*balloon) { return balloon->z; }
int pzGetBalloonTextRgb(Game*game,Balloon*balloon) {
  RGB *textRgb;
  textRgb = &game->board->palette.rgb[balloon->color];
  return PackRgbTo24Bit(*textRgb);
}
double pzGetBalloonOpacity(Balloon*balloon) { return balloon->opacity; }

const char* pzGetMoveAsXmlString(Game*game) {
  xmlBufferPtr buf;
  xmlTextWriterPtr writer;
  buf = NULL;
  /* TODO: allocate buf */
  writer = xmlNewTextWriterMemory(buf, 0);
  writeMoveList (game->board->moveLog, writer, (xmlChar*) XMLZOO_LOG);
  /* TODO: get the string and return it */
  return NULL;
}

const char* pzSaveBoardAsXmlString(Game*game) {
  xmlBufferPtr buf;
  xmlTextWriterPtr writer;
  buf = NULL;
  /* TODO: allocate buf */
  writer = xmlNewTextWriterMemory(buf, 0);
  writeBoard (game->board, writer, 1);
  /* TODO: get the string and return it */
  return NULL;
}
