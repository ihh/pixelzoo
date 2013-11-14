#ifndef CHIBI_INCLUDED
#define CHIBI_INCLUDED

#include "chibi/sexp.h"
#include "chibi/eval.h"

/* Chibi Scheme helpers */
const char* sexp_string_data_wrapper (sexp x);
const char* sexp_string (sexp ctx, sexp x);  /* writes x to string, caller must destroy */

/* Initialize PixelZoo Scheme API */
void sexp_init_lib_board (sexp ctx, sexp env);


#endif /* CHIBI_INCLUDED */
