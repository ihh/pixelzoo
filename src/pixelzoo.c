#include <string.h>

#include "pixelzoo.h"
#include "pzutil.h"
#include "xmlgame.h"
#include "xmlmove.h"
#include "xmlparser.h"

#define MAX_PROPORTION_TIME_EVOLVING .9  /* so that gameLoop doesn't eat 100% of the time between updates */

pzGame pzNewGameFromXmlString(const char*gameString,int logMoves) {
  pzGame pzg;
  pzg = (pzGame) newGameFromXmlString(gameString);
  if (logMoves)
    logBoardMoves (((Game*)pzg)->board);
  return pzg;
}

pzGame pzNewGameAndBoardFromXmlStrings(const char*gameString,const char*boardString,int logMoves) {
  pzGame pzg;
  pzg = (pzGame) newGameFromXmlStringWithSeparateBoard (gameString, boardString);
  if (logMoves)
    logBoardMoves (((Game*)pzg)->board);
  return pzg;
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

void pzQuitGame(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  gameQuit(game);
}

int pzGameRunning(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  return game != NULL && game->gameState == GameRunning;
}

void pzUpdateGame(pzGame pzg,int callsPerSecond,long long boardClockTimeLimit) {
  double targetTicks, maxUpdateTimeInSeconds, actualTicks, elapsedTime;
  int64_Microticks targetMicroticks, actualMicroticks;
  int cellUpdates;
  Game* game;
  game = (Game*) pzg;

  if (callsPerSecond > 0) {
    targetTicks = game->ticksPerSecond / (double) callsPerSecond;
    maxUpdateTimeInSeconds = MAX_PROPORTION_TIME_EVOLVING * targetTicks / game->ticksPerSecond;
    targetMicroticks = FloatToIntMillionths(targetTicks);
    if (boardClockTimeLimit > 0)
      targetMicroticks = MIN (targetMicroticks, boardClockTimeLimit - game->board->microticks);
  } else {
    maxUpdateTimeInSeconds = -1;
    targetMicroticks = MAX (0, boardClockTimeLimit - game->board->microticks);
  }

  if (targetMicroticks > 0)
    innerGameLoop (game, targetMicroticks, maxUpdateTimeInSeconds, &actualMicroticks, &actualTicks, &cellUpdates, &elapsedTime);
}

unsigned long long pzBoardClock (pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  return game->board->microticks;
}

double pzBoardMicroticksPerSecond(pzGame pzg) {
  Game* game;
  game = (Game*) pzg;
  return game->ticksPerSecond * (1 << 20);
}

int pzGetBoardSize(pzGame pzg) {
    Game* game;
    game = (Game*) pzg;
    return game->board->size;
}

int pzGetBoardDepth(pzGame pzg) {
    Game* game;
    game = (Game*) pzg;
    return game->board->depth;
}

int pzGetCellRgb(pzGame pzg,int x,int y,int z) {
  RGB *cellRgb;
  Game* game;
  game = (Game*) pzg;
  cellRgb = &game->board->palette.rgb[pzGetCellPaletteIndex(pzg, x, y, z)];
  return PackRgbTo24Bit(*cellRgb);
}

int pzGetCellPaletteIndex(pzGame pzg,int x,int y,int z) {
  PaletteIndex cellColorIndex;
  Game* game;
  game = (Game*) pzg;
  cellColorIndex = readBoardColor(game->board, x, y, z);
  return (int) cellColorIndex;
}

int*** pzNewCellRgbArray(pzGame pzg) {
  int x, y, size, depth;
  int*** cell;
  Game* game;
  game = (Game*) pzg;
  size = game->board->size;
  depth = game->board->depth;
  cell = SafeCalloc (size, sizeof(int*));
  for (x = 0; x < size; ++x) {
    cell[x] = SafeCalloc (size, sizeof(int*));
    for (y = 0; y < size; ++y)
      cell[x][y] = SafeCalloc (depth, sizeof(int));
  }
  return cell;
}

void pzDeleteCellRgbArray(pzGame pzg, int*** cell) {
  int x, y, size;
  Game* game;
  game = (Game*) pzg;
  size = game->board->size;
  for (x = 0; x < size; ++x) {
    for (y = 0; y < size; ++y)
      SafeFree (cell[x][y]);
    SafeFree (cell[x]);
  }
  SafeFree (cell);
}

void pzReadCellRgbArray(pzGame pzg,int*** cell) {
  int x, y, z, size, depth;
  Game* game;
  game = (Game*) pzg;
  size = game->board->size;
  depth = game->board->depth;
  for (x = 0; x < size; ++x)
    for (y = 0; y < size; ++y)
      for (z = 0; z < depth; ++z)
	cell[x][y][z] = pzGetCellRgb (pzg, x, y, z);
}

const char* pzGetCellName(pzGame pzg,int x,int y,int z) {
  State examState;
  Particle *particle;
  char *text;
  Game* game;
  game = (Game*) pzg;
  examState = readBoardState (game->board, x, y, z);
  particle = game->board->byType[StateType(examState)];
  text = particle ? particle->name : NULL;
  return text;
}

int pzGetCellNameRgb(pzGame pzg,int x,int y,int z) {
  State examState;
  Particle *particle;
  PaletteIndex examColorIndex;
  RGB *cellNameRgb;
  Game* game;
  game = (Game*) pzg;
  examState = readBoardState (game->board, x, y, z);
  particle = game->board->byType[StateType(examState)];
  examColorIndex = particle ? getParticleColor (particle, examState) : PaletteWhite;
  cellNameRgb = &game->board->palette.rgb[examColorIndex];
  return PackRgbTo24Bit(*cellNameRgb);
}

const char* pzGetCellSprite(pzGame pzg,int x,int y,int z) {
  State s;
  Particle *particle;
  char *text;
  Game* game;
  game = (Game*) pzg;
  s = readBoardState (game->board, x, y, z);
  particle = game->board->byType[StateType(s)];
  text = particle ? particle->sprite : NULL;
  return text;
}


int pzGetNumberOfTools(pzGame pzg) {
  ListNode *toolNode;
  int nTools;
  Game* game;
  game = (Game*) pzg;
  nTools = 0;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next)
    ++nTools;
  return nTools;
}

pzTool pzGetToolByNumber(pzGame pzg,int toolNum) {
  ListNode *toolNode;
  Tool *tool;
  Game* game;
  game = (Game*) pzg;
  for (toolNode = game->toolOrder->head; toolNode != NULL; toolNode = toolNode->next) {
    tool = toolNode->value;
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

const char* pzGetToolIcon(pzTool pzt) {
    const char *toolIcon;
    Tool* tool;
    tool = (Tool*) pzt;
    toolIcon = tool ? tool->icon : NULL;
    return toolIcon;
}

int pzGetSelectedToolNumber(pzGame pzg) {
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
      if (tool == game->selectedTool) {
	selectedToolNum = toolNum;
	break;
      }
      ++toolNum;
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

const int PZEXPORT pzGetToolZ(pzTool pzt) {
    Tool* tool;
    tool = (Tool*) pzt;
    return tool->z;
}

void pzSelectTool(pzGame pzg,int toolNum) {
  Game* game;
  game = (Game*) pzg;
  game->selectedTool = pzGetToolByNumber (game, toolNum);
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


const char* pzSaveMoveAsXmlString(pzGame pzg) {
  xmlTextWriterPtr writer;
  const char* str;
  Game* game;
  game = (Game*) pzg;
  boardReleaseRandomNumbers (game->board);
  str = NULL;
  if (game->board->moveLog)
    {
      writer = newXmlTextWriter();
      if (writer) {
	writeMoveList (game->board->moveLog, writer, (xmlChar*) XMLZOO_LOG);
	str = (const char*) deleteXmlTextWriterLeavingText (writer);
      }
    }
  return str;
}

const char* pzSaveBoardAsXmlString(pzGame pzg) {
  xmlTextWriterPtr writer;
  const char* str;
  Game* game;
  game = (Game*) pzg;
  str = NULL;
  boardReleaseRandomNumbers (game->board);
  writer = newXmlTextWriter();
  if (writer) {
    writeBoardXml (game->board, writer, 1);
    str = (const char*) deleteXmlTextWriterLeavingText (writer);
  }
  return str;
}


const char* pzSaveBoardAndMoveAsXmlString(pzGame pzg) {
  xmlTextWriterPtr writer;
  const char* str;
  Game* game;
  game = (Game*) pzg;
  str = NULL;
  boardReleaseRandomNumbers (game->board);
  writer = newXmlTextWriter();
  if (writer) {
    xmlTextWriterStartElement (writer, (xmlChar*) XMLZOO_GAME);
    writeMoveList (game->board->moveLog, writer, (xmlChar*) XMLZOO_LOG);
    writeBoardXml (game->board, writer, 1);
    xmlTextWriterEndElement (writer);  /* end game element */
    str = (const char*) deleteXmlTextWriterLeavingText (writer);
  }
  return str;
}


int pzGetPaletteSize(pzGame pzg) { return PaletteSize; }

int pzGetPaletteRgb(pzGame pzg,int paletteIndex) {
  RGB rgb;
  Game* game;
  game = (Game*) pzg;
  rgb = game->board->palette.rgb[paletteIndex];
  return PackRgbTo24Bit (rgb);
}

void pzAbort(char* error) { Abort(error); }
void pzAssert(int assertion, char* error) { Assert(assertion,error); }

int pzIncumbentCount(pzGame pzg) {
  Game *game;
  game = (Game*) pzg;
  return boardContestParticleCount (game->board, game->board->incumbentWinVar);
}

int pzChallengerCount(pzGame pzg) {
  Game *game;
  game = (Game*) pzg;
  return boardContestParticleCount (game->board, game->board->challengerWinVar);
}
