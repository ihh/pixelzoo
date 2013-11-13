#define sexp_init_library sexp_init_lib_srfi_95
#include "lib/srfi/95/qsort.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_srfi_69
#include "lib/srfi/69/hash.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_srfi_39
#include "lib/srfi/39/param.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_srfi_33
#include "lib/srfi/33/bit.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_srfi_27
#include "lib/srfi/27/rand.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_srfi_18
#include "lib/srfi/18/threads.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_scheme_time
#include "lib/scheme/time.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_weak
#include "lib/chibi/weak.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_time
#include "lib/chibi/time.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_system
#include "lib/chibi/system.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_stty
#include "lib/chibi/stty.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_optimize_rest
#include "lib/chibi/optimize/rest.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_optimize_profile
#include "lib/chibi/optimize/profile.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_net
#include "lib/chibi/net.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_io
#include "lib/chibi/io/io.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_heap_stats
#include "lib/chibi/heap-stats.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_filesystem
#include "lib/chibi/filesystem.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_disasm
#include "lib/chibi/disasm.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_chibi_ast
#include "lib/chibi/ast.c"
#undef sexp_init_library

typedef struct {
  const char *name;
  sexp_init_proc init;
} sexp_library_entry_t;

static sexp_library_entry_t sexp_static_libraries[] = {
  { "lib/srfi/95/qsort", sexp_init_lib_srfi_95 },
  { "lib/srfi/69/hash", sexp_init_lib_srfi_69 },
  { "lib/srfi/39/param", sexp_init_lib_srfi_39 },
  { "lib/srfi/33/bit", sexp_init_lib_srfi_33 },
  { "lib/srfi/27/rand", sexp_init_lib_srfi_27 },
  { "lib/srfi/18/threads", sexp_init_lib_srfi_18 },
  { "lib/scheme/time", sexp_init_lib_scheme_time },
  { "lib/chibi/weak", sexp_init_lib_chibi_weak },
  { "lib/chibi/time", sexp_init_lib_chibi_time },
  { "lib/chibi/system", sexp_init_lib_chibi_system },
  { "lib/chibi/stty", sexp_init_lib_chibi_stty },
  { "lib/chibi/optimize/rest", sexp_init_lib_chibi_optimize_rest },
  { "lib/chibi/optimize/profile", sexp_init_lib_chibi_optimize_profile },
  { "lib/chibi/net", sexp_init_lib_chibi_net },
  { "lib/chibi/io/io", sexp_init_lib_chibi_io },
  { "lib/chibi/heap-stats", sexp_init_lib_chibi_heap_stats },
  { "lib/chibi/filesystem", sexp_init_lib_chibi_filesystem },
  { "lib/chibi/disasm", sexp_init_lib_chibi_disasm },
  { "lib/chibi/ast", sexp_init_lib_chibi_ast },
  { NULL, NULL }
};

