#ifndef RULE_INCLUDED
#define RULE_INCLUDED

/* 32-bit cell state.

   The 16-bit MSW (most significant word) of the state is the Type.
   The 16-bit LSW (least significant word) provides additional type-specific state.

   All states of type zero (i.e. MSW=0) have zero update rate.
   State 0x00000000, where LSW=MSW=0, is assumed to be empty space.
   States with MSW=0 and LSW>0 are reserved (including e.g. scenery grayscale & colors, see below).

   A common convention is that the state with LSW=0 is "prototypical" for that MSW,
   but this convention is not strictly enforced or required.
 */
typedef unsigned long State;
#define StateMask 0xffffffff
#define BitsPerState 32

/* 16-bit cell type */
typedef unsigned short Type;
#define TypeMask    0xffff0000
#define TypeShift   16
#define NumTypes    0x10000
#define MaxType     0xffff
#define BitsPerType 16

/* type <-> state conversion macros */
#define StateType(STATE) (((STATE) & TypeMask) >> TypeShift)

/* Reserved states & types */
/* Empty (aka "void", "scenery black") */
#define EmptyState 0
#define EmptyType  0

/* Scenery grayscale pixels */
#define SceneryGrayLevels    256     /* number of scenery grayscale levels, including black */
#define SceneryGrayMin       0x00    /* offset for scenery grayscale levels, starting at black (empty space) */
#define SceneryGrayMax       0xff    /* SceneryGrayscaleMin + SceneryGrays */

/* Scenery color pixels - high-saturation
   (hue H,bright B) => state H + SceneryColorHues * B + SceneryColorMin
*/
#define SceneryColorHues     256
#define SceneryColorBrights  8
#define SceneryColors        0x800    /* SceneryColorHues * SceneryColorBrights */
#define SceneryColorMin      0x100
#define SceneryColorMax      0x8ff    /* SceneryColorMin + SceneryColors - 1 */

/* Scenery color pixels - low-saturation
   (hue H,saturation S,bright B) => state H + SceneryPaleColorHues * (B + SceneryPaleColorBrights * S) + SceneryPaleColorMin
*/
#define SceneryPaleColorHues         64
#define SceneryPaleColorBrights      2
#define SceneryPaleColorSaturations  4
#define SceneryPaleColors            0x200    /* SceneryPaleColorHues * SceneryPaleColorBrights * SceneryPaleColorSaturations */
#define SceneryPaleColorMin          0x900
#define SceneryPaleColorMax          0xaff    /* SceneryPaleColorMin + SceneryColors - 1 */

/* Scenery color pixels - state boundaries */
#define SceneryMax      SceneryPaleColorMax
#define SceneryStates   0xb00

/* macros for scenery states */
#define GrayScenery(GrayLevel) ((255 * (GrayLevel) / (SceneryGrayLevels - 1)) % SceneryGrayLevels)
#define ColorScenery(Hue,Bright) ((((Hue) + SceneryColorHues * (Bright)) % SceneryColors) + SceneryColorMin)
#define PaleColorScenery(PaleHue,PaleSaturation,PaleBright) \
 ((((PaleHue) + SceneryPaleColorHues * ((PaleBright) + SceneryPaleColorBrights * (PaleSaturation))) % SceneryPaleColors) + SceneryPaleColorMin)


/* Short-range relative co-ordinate offset
   1 byte each for X & Y
 */
typedef struct LocalOffset {
  char x, y;
} LocalOffset;

/*
  RuleCondition describes the following conditional test:
   (randomDouble() < ignoreProb)  ||  (cell[loc] & mask  <opcode>  rhs)
*/
typedef struct RuleCondition {
  LocalOffset loc;
  State mask, rhs;
  enum ConditionalOpcode { EQ, NEQ, GT, LT, GEQ, LEQ, TRUE, FALSE } opcode;
  double ignoreProb, overloadIgnoreProb;  /* when board (or local region) is overloaded, overloadIgnoreProb will be used instead of ignoreProb */
} RuleCondition;

/*
  RuleOperation describes the following operation:
  if (randomDouble() >= failProb)
    cell[dest] = (cell[dest] & (StateMask ^ (mask << leftShift))) | ((((cell[src] >> rightShift) + offset) & mask) << leftShift);

  In practice, it is implemented as follows
  if (randomDouble() >= failProb)
    cell[dest] = (cell[dest] & (StateMask ^ (mask << leftShift))) | (((((cell[src] & preMask) >> rightShift) + offset) & mask) << leftShift);

  where
    preMask = (rightShift < 32) ? StateMask : 0

  Why is preMask necessary? Well, I think that "x >> 32" should equal zero if x is a 32-bit number,
  and on many computers/compilers it indeed seems to, but I swear that on my first-generation MacBook Air, it equaled "x".
  "x>>y" worked fine on that computer for y<32, and "x>>32" equaled zero as expected in gdb, and on another laptop (2nd-gen MacBook Air).
  Could be a gcc problem, could be a voodoo chicken fluke. Who knows.
  BIZARRE.
*/
typedef struct RuleOperation {
  LocalOffset src, dest;
  unsigned char rightShift, leftShift;
  State offset, mask, preMask;
  double failProb, overloadFailProb;  /* when board (or local region) is overloaded, overloadFailProb will be used instead of failProb */
} RuleOperation;

/*
  A StochasticRule (associated with a given Particle) consists of:
   a fixed number of RuleCondition's,
   a fixed number of RuleOperation's,
   a firing rate (corresponding to the relative rate that this rule will be selected out of all rules applying to this Type)
   an overloaded firing rate (the firing rate that will be used when the board is too full)
 */

/* first define the size of the condition & operation blocks */
#define NumRuleConditions 6
#define NumRuleOperations 6

/* now the struct */
typedef struct StochasticRule {
  RuleCondition cond[NumRuleConditions];
  RuleOperation op[NumRuleOperations];
  double rate, overloadRate;
} StochasticRule;

#endif /* RULE_INCLUDED */
