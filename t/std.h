#define RandomStep(DX,DY,STEP,OSTEP,DIE,ODIE)			\
  {								\
    <rate STEP>;						\
    <overload OSTEP>;						\
    loc orig(0,0);						\
    loc next(DX,DY);						\
    temp swap;							\
    if next.type = empty;					\
    do swap = next;						\
    do next = orig <fail (DIE/STEP)> <overload (ODIE/OSTEP)>;	\
    do orig = swap;						\
  }								\

#define MooreWalk(STEP,OSTEP,DIE,ODIE)		\
  RandomStep(+1,0,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));		\
  RandomStep(-1,0,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));		\
  RandomStep(0,+1,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));		\
  RandomStep(0,-1,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));		\

#define BishopWalk(STEP,OSTEP,DIE,ODIE)		\
  RandomStep(+1,+1,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));	\
  RandomStep(-1,+1,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));	\
  RandomStep(+1,-1,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));	\
  RandomStep(-1,-1,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));	\

#define NeumannWalk(STEP,OSTEP,DIE,ODIE)	\
  MooreWalk((STEP/2),(OSTEP/2),(DIE/2),(ODIE/2));		\
  BishopWalk((STEP/2),(OSTEP/2),(DIE/2),(ODIE/2));		\

#define DirectedStep(DX,DY,DIR,STEP,OSTEP,DIE,ODIE)	\
  {							\
    <rate STEP>;					\
    <overload OSTEP>;					\
    loc orig(0,0);					\
    loc next(DX,DY);					\
    temp swap;						\
    if orig.dir = DIR;					\
    if next.type = empty;				\
    do swap = next;					\
    do next = orig <fail DIE> <overload ODIE>;		\
    do orig = swap;					\
  }							\

#define DirectedTurn(TYPE,DX,DY,DIR,DIRINC,RATE,SPONTANEOUS)	\
  {								\
    <rate RATE>;						\
    loc orig(0,0);						\
    loc next(DX,DY);						\
    if orig.dir = DIR;						\
    if next.type != empty <ignore (SPONTANEOUS/RATE)>;		\
    if next.type != TYPE <ignore (SPONTANEOUS/RATE)>;		\
    do orig.dir = orig.dir + DIRINC;				\
  }								\

#define DirectedTurns(TYPE,DX,DY,DIR,TURN,REVERSE,SPONTANEOUS)		\
  DirectedTurn(TYPE,DX,DY,DIR,+1,(TURN/2),(SPONTANEOUS/3));		\
  DirectedTurn(TYPE,DX,DY,DIR,-1,(TURN/2),(SPONTANEOUS/3));		\
  DirectedTurn(TYPE,DX,DY,DIR,+2,REVERSE,(SPONTANEOUS/3));		\

// dir: 0=north, 1=east, 2=south, 3=west
// Moore topology random walk with direction
#define MooreDirectedWalk(TYPE,STEP,OSTEP,DIE,ODIE,TURN,REVERSE,SPONTANEOUS) \
  DirectedStep(0,-1,0,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));				\
  DirectedStep(+1,0,1,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));				\
  DirectedStep(0,+1,2,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));				\
  DirectedStep(-1,0,3,(STEP/4),(OSTEP/4),(DIE/4),(ODIE/4));				\
  DirectedTurns(TYPE,0,-1,0,(TURN/4),(REVERSE/4),(SPONTANEOUS/4));			\
  DirectedTurns(TYPE,+1,0,1,(TURN/4),(REVERSE/4),(SPONTANEOUS/4));			\
  DirectedTurns(TYPE,0,+1,2,(TURN/4),(REVERSE/4),(SPONTANEOUS/4));			\
  DirectedTurns(TYPE,-1,0,3,(TURN/4),(REVERSE/4),(SPONTANEOUS/4));			\

// flock
#define FlockDir(TYPE,X,Y,RATE)				\
  {							\
  <rate RATE>;						\
  loc orig(0,0);					\
  loc nbr(X,Y);						\
  if nbr.type = TYPE;					\
  do orig.dir = nbr.dir;				\
  }							\

#define Flock1(TYPE,RATE)					\
  FlockDir(TYPE,-1,-1,(RATE/8));				\
  FlockDir(TYPE,-1,0,(RATE/8));					\
  FlockDir(TYPE,-1,+1,(RATE/8));				\
  FlockDir(TYPE,0,-1,(RATE/8));					\
  FlockDir(TYPE,0,+1,(RATE/8));					\
  FlockDir(TYPE,+1,-1,(RATE/8));				\
  FlockDir(TYPE,+1,0,(RATE/8));					\
  FlockDir(TYPE,+1,+1,(RATE/8));				\

#define Flock2(TYPE,RATE)					\
  Flock1(TYPE,(RATE/3));					\
  FlockDir(TYPE,-2,-2,(RATE/24));				\
  FlockDir(TYPE,-2,-1,(RATE/24));				\
  FlockDir(TYPE,-2,0,(RATE/24));				\
  FlockDir(TYPE,-2,+1,(RATE/24));				\
  FlockDir(TYPE,-2,+2,(RATE/24));				\
  FlockDir(TYPE,-1,-2,(RATE/24));				\
  FlockDir(TYPE,-1,+2,(RATE/24));				\
  FlockDir(TYPE,0,-2,(RATE/24));				\
  FlockDir(TYPE,0,+2,(RATE/24));				\
  FlockDir(TYPE,+1,-2,(RATE/24));				\
  FlockDir(TYPE,+1,+2,(RATE/24));				\
  FlockDir(TYPE,+2,-2,(RATE/24));				\
  FlockDir(TYPE,+2,-1,(RATE/24));				\
  FlockDir(TYPE,+2,0,(RATE/24));				\
  FlockDir(TYPE,+2,+1,(RATE/24));				\
  FlockDir(TYPE,+2,+2,(RATE/24));				\

// "strafe" (sidestep)
#define StrafeDir(DX,DY,DIR,STRAFEX,STRAFEY,RATE)	\
  {							\
  <rate RATE>;						\
  loc orig(0,0);					\
  loc next(DX,DY);					\
  loc strafe(STRAFEX,STRAFEY);				\
  if orig.dir = DIR;					\
  if next.type != empty;				\
  if strafe.type = empty;				\
  do strafe = orig;					\
  do orig = empty;					\
  }							\

#define Strafe(RATE)					\
  StrafeDir(0,-1,0,+1,0,(RATE/8));			\
  StrafeDir(0,-1,0,-1,0,(RATE/8));			\
  StrafeDir(+1,0,1,0,+1,(RATE/8));			\
  StrafeDir(+1,0,1,0,-1,(RATE/8));			\
  StrafeDir(0,+1,2,-1,0,(RATE/8));			\
  StrafeDir(0,+1,2,+1,0,(RATE/8));			\
  StrafeDir(-1,0,3,0,-1,(RATE/8));			\
  StrafeDir(-1,0,3,0,+1,(RATE/8));			\

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

