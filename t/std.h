#define RandomStep(DX,DY,STEP,OSTEP,TEST,DIE,ODIE,BREED,OBREED,EXEC)	\
  {									\
    <rate STEP>;							\
    <overload OSTEP>;							\
    loc orig(0,0);							\
    loc next(DX,DY);							\
    if next.type = empty;						\
    TEST;								\
    do next = orig <fail (DIE/STEP)> <overload (ODIE/OSTEP)>;		\
    do orig = empty <fail (BREED/STEP)> <overload (OBREED/OSTEP)>;	\
    EXEC;								\
  }									\

#define MooreWalk(STEP,OSTEP,TEST,DIE,ODIE,BREED,OBREED,EXEC)	\
  RandomStep(+1,0,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC); \
  RandomStep(-1,0,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC);		\
  RandomStep(0,+1,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC);		\
  RandomStep(0,-1,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC);		\

#define BishopWalk(STEP,OSTEP,TEST,DIE,ODIE,BREED,OBREED,EXEC)	\
  RandomStep(+1,+1,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC);	\
  RandomStep(-1,+1,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC);	\
  RandomStep(+1,-1,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC);	\
  RandomStep(-1,-1,(STEP/4),(OSTEP/4),TEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),EXEC);	\

#define NeumannWalk(STEP,OSTEP,TEST,DIE,ODIE,BREED,OBREED,EXEC)		\
  MooreWalk((STEP/2),(OSTEP/2),TEST,(DIE/2),(ODIE/2),(BREED/2),(OBREED/2),EXEC); \
  BishopWalk((STEP/2),(OSTEP/2),TEST,(DIE/2),(ODIE/2),(BREED/2),(OBREED/2),EXEC); \

#define MooreDirectedStep(DX,DY,DIR,STEP,OSTEP,TEST,DIE,ODIE,BREED,OBREED,EXEC) \
  {									\
    <rate STEP>;							\
    <overload OSTEP>;							\
    loc orig(0,0);							\
    loc next(DX,DY);							\
    if orig.dir = DIR;							\
    if next.type = empty;						\
    TEST;								\
    do next = orig <fail (DIE/STEP)> <overload (ODIE/OSTEP)>;		\
    do orig = empty <fail (BREED/STEP)> <overload (OBREED/OSTEP)>;	\
    EXEC;								\
  }									\

#define MooreDirectedTurn(TYPE,DX,DY,DIR,TEST,DIRINC,RATE,SPONTANEOUS,EXEC)	\
  {								\
    <rate RATE>;						\
    loc orig(0,0);						\
    loc next(DX,DY);						\
    if orig.dir = DIR;						\
    if next.type != empty <ignore (SPONTANEOUS/RATE)>;		\
    if next.type != TYPE <ignore (SPONTANEOUS/RATE)>;		\
    TEST;							\
    do orig.dir = orig.dir + DIRINC;				\
    EXEC;							\
  }								\

#define MooreDirectedTurns(TYPE,DX,DY,DIR,TEST,TURN,REVERSE,SPONTANEOUS,EXEC) \
  MooreDirectedTurn(TYPE,DX,DY,DIR,TEST,+1,(TURN/2),(SPONTANEOUS/3),EXEC);	\
  MooreDirectedTurn(TYPE,DX,DY,DIR,TEST,-1,(TURN/2),(SPONTANEOUS/3),EXEC);	\
  MooreDirectedTurn(TYPE,DX,DY,DIR,TEST,+2,REVERSE,(SPONTANEOUS/3),EXEC);	\
									\
// dir: 0=north, 1=east, 2=south, 3=west
// Moore topology random walk with direction
#define MooreDirectedWalk(TYPE,STEP,OSTEP,STEPTEST,DIE,ODIE,BREED,OBREED,STEPEXEC,TURNTEST,TURN,REVERSE,SPONTANEOUS,TURNEXEC) \
  MooreDirectedStep(0,-1,0,(STEP/4),(OSTEP/4),STEPTEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),STEPEXEC); \
  MooreDirectedStep(+1,0,1,(STEP/4),(OSTEP/4),STEPTEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),STEPEXEC); \
  MooreDirectedStep(0,+1,2,(STEP/4),(OSTEP/4),STEPTEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),STEPEXEC); \
  MooreDirectedStep(-1,0,3,(STEP/4),(OSTEP/4),STEPTEST,(DIE/4),(ODIE/4),(BREED/4),(OBREED/4),STEPEXEC); \
  MooreDirectedTurns(TYPE,0,-1,0,TURNTEST,(TURN/4),(REVERSE/4),(SPONTANEOUS/4),TURNEXEC); \
  MooreDirectedTurns(TYPE,+1,0,1,TURNTEST,(TURN/4),(REVERSE/4),(SPONTANEOUS/4),TURNEXEC); \
  MooreDirectedTurns(TYPE,0,+1,2,TURNTEST,(TURN/4),(REVERSE/4),(SPONTANEOUS/4),TURNEXEC); \
  MooreDirectedTurns(TYPE,-1,0,3,TURNTEST,(TURN/4),(REVERSE/4),(SPONTANEOUS/4),TURNEXEC); \
									\
// flock
#define MooreFlockDir(TYPE,X,Y,RATE,CODE)			\
  {							\
    <rate RATE>;						\
    loc orig(0,0);					\
    loc nbr(X,Y);						\
    if nbr.type = TYPE;					\
    do orig.dir = nbr.dir;				\
    CODE;							\
  }							\

#define MooreFlock1(TYPE,RATE,CODE)					\
  MooreFlockDir(TYPE,-1,-1,(RATE/8),CODE);				\
  MooreFlockDir(TYPE,-1,0,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,-1,+1,(RATE/8),CODE);				\
  MooreFlockDir(TYPE,0,-1,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,0,+1,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,+1,-1,(RATE/8),CODE);				\
  MooreFlockDir(TYPE,+1,0,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,+1,+1,(RATE/8),CODE);				\

#define MooreFlock2(TYPE,RATE,CODE)			\
  MooreFlock1(TYPE,(RATE/3),CODE);			\
  MooreFlockDir(TYPE,-2,-2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,-2,-1,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,-2,0,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,-2,+1,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,-2,+2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,-1,-2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,-1,+2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,0,-2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,0,+2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,+1,-2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,+1,+2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,+2,-2,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,+2,-1,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,+2,0,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,+2,+1,(RATE/24),CODE);		\
  MooreFlockDir(TYPE,+2,+2,(RATE/24),CODE);		\
						\

// "strafe" (sidestep)
#define MooreStrafeDir(DX,DY,DIR,STRAFEX,STRAFEY,RATE,CODE)	\
  {							\
    <rate RATE>;					\
    loc orig(0,0);					\
    loc next(DX,DY);					\
    loc strafe(STRAFEX,STRAFEY);			\
    if orig.dir = DIR;					\
    if next.type != empty;				\
    if strafe.type = empty;				\
    do strafe = orig;					\
    do orig = empty;					\
    CODE;						\
  }							\

#define MooreStrafe(RATE,CODE)				\
  MooreStrafeDir(0,-1,0,+1,0,(RATE/8),CODE);			\
  MooreStrafeDir(0,-1,0,-1,0,(RATE/8),CODE);			\
  MooreStrafeDir(+1,0,1,0,+1,(RATE/8),CODE);			\
  MooreStrafeDir(+1,0,1,0,-1,(RATE/8),CODE);			\
  MooreStrafeDir(0,+1,2,-1,0,(RATE/8),CODE);			\
  MooreStrafeDir(0,+1,2,+1,0,(RATE/8),CODE);			\
  MooreStrafeDir(-1,0,3,0,-1,(RATE/8),CODE);			\
  MooreStrafeDir(-1,0,3,0,+1,(RATE/8),CODE);			\

#define BuildDirStep(DX,DY,DIR,COORD,LIMIT,INC,DIRFLAG,REVFLAG,RATE)	\
  {									\
    <rate (RATE/3)>;							\
    loc orig(0,0);							\
    loc nbr(DX,DY);							\
    if orig.dir = DIR;							\
    if orig.DIRFLAG != 0;						\
    if orig.COORD != LIMIT;						\
    if nbr.type = empty;						\
    do nbr = orig;							\
    do nbr.COORD = nbr.COORD + INC;					\
  }									\
  {									\
    <rate (RATE/3)>;							\
    loc orig(0,0);							\
    loc nbr(DX,DY);							\
    if orig.dir = DIR;							\
    if nbr.type != empty;						\
    do orig.DIRFLAG = 0;						\
  }									\
  {									\
    <rate (RATE/3)>;							\
    loc orig(0,0);							\
    if orig.COORD = LIMIT;						\
    do orig.DIRFLAG = 0;						\
  }									\

#define BuildDir(DX,DY,NEG_DX,NEG_DY,DIR,MAXCOL,MAXROW,RATE)		\
  BuildDirStep(DX,DY,DIR,row,0,-1,buildForward,buildBackward,(RATE/4));		\
  BuildDirStep(NEG_DY,DX,DIR,col,MAXCOL,+1,buildRight,buildLeft,(RATE/4)); \
  BuildDirStep(NEG_DX,NEG_DY,DIR,row,MAXROW,+1,buildBackward,buildForward,(RATE/4)); \
  BuildDirStep(DY,NEG_DX,DIR,col,0,-1,buildLeft,buildRight,(RATE/4));	\

#define Build(MAXCOL,MAXROW,RATE)			\
  BuildDir(0,-1,0,+1,0,MAXCOL,MAXROW,(RATE/4));		\
  BuildDir(+1,0,-1,0,1,MAXCOL,MAXROW,(RATE/4));		\
  BuildDir(0,+1,0,-1,2,MAXCOL,MAXROW,(RATE/4));		\
  BuildDir(-1,0,+1,0,3,MAXCOL,MAXROW,(RATE/4));		\

#define EndBuild(TEST,EXEC,RATE)		\
  {						\
    <rate RATE>;				\
    loc orig(0,0);				\
    temp builder;				\
    if orig.buildForward = 0;			\
    if orig.buildBackward = 0;			\
    if orig.buildRight = 0;			\
    if orig.buildLeft = 0;			\
    TEST;					\
    do builder = orig;				\
    EXEC;					\
  }						\

