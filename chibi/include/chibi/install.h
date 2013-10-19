#define sexp_so_extension ".dylib"

/* Need to define sexp_default_module_path to point to lib/chibi in Makefile, project build settings, etc. */
#ifndef sexp_default_module_path
extern char* sexp_default_module_path;
#endif

/* Similarly, need to define sexp_pixelzoo_module_path to point to ../scheme/zoo.scm */
#ifndef sexp_pixelzoo_module_path
extern char* sexp_pixelzoo_module_path;
#endif

#define sexp_platform "pixelzoo"
#define sexp_version "0.5.2"
#define sexp_release_name "boron"
