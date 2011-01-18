// colors
#define RedHue    0
#define YellowHue 42
#define GreenHue  84
#define CyanHue   126
#define BlueHue   168
#define PinkHue   210

#define Bright    bri = 255; sat = 255;

#define BrightRed    hue = RedHue; Bright;
#define BrightYellow hue = YellowHue; Bright;
#define BrightGreen  hue = GreenHue; Bright;
#define BrightCyan   hue = CyanHue; Bright;
#define BrightBlue   hue = BlueHue; Bright;
#define BrightPink   hue = PinkHue; Bright;

// rules involving self only
#define SelfAction(RATE,CODE)	\
  {				\
    <rate RATE>;		\
    loc self(0,0);		\
    CODE;			\
  }				\

#define SelfTransform(RATE,TYPE,TEST,EXEC)	\
  SelfAction(RATE,TEST;do self = TYPE;EXEC;)

// lhs-pair rules involving a target
#define TargetRule(DX,DY,RATE,CODE)		\
  {						\
    <rate RATE>;				\
    loc src(0,0);				\
    loc tgt(DX,DY);				\
    CODE;					\
  }						\

// topologies
#define MooreRule(RATE,CODE)	\
  TargetRule(+1,0,(RATE/4),CODE);	\
  TargetRule(-1,0,(RATE/4),CODE);	\
  TargetRule(0,+1,(RATE/4),CODE);	\
  TargetRule(0,-1,(RATE/4),CODE);	\

#define BishopRule(RATE,CODE)	\
  TargetRule(+1,+1,(RATE/4),CODE);	\
  TargetRule(-1,-1,(RATE/4),CODE);	\
  TargetRule(-1,+1,(RATE/4),CODE);	\
  TargetRule(+1,-1,(RATE/4),CODE);	\

#define NeumannRule(RATE,CODE)		\
  MooreRule((RATE/2),CODE);		\
  BishopRule((RATE/2),CODE);		\


// lhs-pair rules of the form A B ... -> ...
#define BindTarget(DX,DY,TYPE,RATE,CODE)	\
  TargetRule(DX,DY,RATE,if tgt.type = TYPE;CODE;)

// topologies
#define MooreBind(TYPE,RATE,CODE)	\
  BindTarget(+1,0,TYPE,(RATE/4),CODE);	\
  BindTarget(-1,0,TYPE,(RATE/4),CODE);	\
  BindTarget(0,+1,TYPE,(RATE/4),CODE);	\
  BindTarget(0,-1,TYPE,(RATE/4),CODE);	\

#define BishopBind(TYPE,RATE,CODE)	\
  BindTarget(+1,+1,TYPE,(RATE/4),CODE);	\
  BindTarget(-1,-1,TYPE,(RATE/4),CODE);	\
  BindTarget(-1,+1,TYPE,(RATE/4),CODE);	\
  BindTarget(+1,-1,TYPE,(RATE/4),CODE);	\

#define NeumannBind(TYPE,RATE,CODE)		\
  MooreBind(TYPE,(RATE/2),CODE);		\
  BishopBind(TYPE,(RATE/2),CODE);		\

// pair rules of the form A B (TEST) -> C D (EXEC)
#define MoorePair(TARGET,RATE,TEST,NEWSOURCE,NEWTARGET,EXEC)		\
  MooreBind(TARGET,RATE,TEST;do src = NEWSOURCE;do tgt = NEWTARGET;EXEC;)

#define NeumannPair(TARGET,RATE,TEST,NEWSOURCE,NEWTARGET,EXEC)		\
  NeumannBind(TARGET,RATE,TEST;do src = NEWSOURCE;do tgt = NEWTARGET;EXEC;)

// Brownian motion random walks
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


// random walks with correlated direction
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

// flocking
#define MooreFlockDir(TYPE,X,Y,RATE,CODE)			\
  {							\
    <rate RATE>;						\
    loc orig(0,0);					\
    loc nbr(X,Y);						\
    if nbr.type = TYPE;					\
    do orig.dir = nbr.dir;				\
    CODE;							\
  }							\

// flocking w/neighbors only (neighborhood size 1)
#define MooreFlock1(TYPE,RATE,CODE)					\
  MooreFlockDir(TYPE,-1,-1,(RATE/8),CODE);				\
  MooreFlockDir(TYPE,-1,0,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,-1,+1,(RATE/8),CODE);				\
  MooreFlockDir(TYPE,0,-1,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,0,+1,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,+1,-1,(RATE/8),CODE);				\
  MooreFlockDir(TYPE,+1,0,(RATE/8),CODE);					\
  MooreFlockDir(TYPE,+1,+1,(RATE/8),CODE);				\

// flocking w/neighbors and next-nearest only (neighborhood size 2)
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

// "strafing" (sidestepping obstacles)
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


// building
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

// build a square
#define Build(MAXCOL,MAXROW,RATE)			\
  BuildDir(0,-1,0,+1,0,MAXCOL,MAXROW,(RATE/4));		\
  BuildDir(+1,0,-1,0,1,MAXCOL,MAXROW,(RATE/4));		\
  BuildDir(0,+1,0,-1,2,MAXCOL,MAXROW,(RATE/4));		\
  BuildDir(-1,0,+1,0,3,MAXCOL,MAXROW,(RATE/4));		\

// do something else after building
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

