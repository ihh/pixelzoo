#include "chibi.h"

const char* sexp_string_data_wrapper (sexp x) {
  return sexp_string_data(x);
}

const char* sexp_string (sexp ctx, sexp x) {
  return sexp_string_data_wrapper (sexp_write_to_string (ctx, x));
}
