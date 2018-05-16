#ifndef TITAN_H
#define TITAN_H

#include "luaconf.h"
#include "lobject.h"
#include "lstring.h"

#ifdef __GNUC__
#define TITAN_LIKELY(x)   __builtin_expect((x), 1)
#define TITAN_UNLIKELY(x) __builtin_expect((x), 0)
#else
#define TITAN_LIKELY(x)   (x)
#define TITAN_UNLIKELY(x) (x)
#endif

#define TITAN_VER          "0.5"
#define TITAN_VER_SUFFIX   "_0_5"
#define TITAN_PATH_VAR     "TITAN_PATH"
#define TITAN_PATH_SEP     "/"
#define TITAN_PATH_DEFAULT ".;/usr/local/lib/titan/" TITAN_VER
#define TITAN_PATH_KEY     "ec10e486-d8fd-11e7-87f4-e7e9581a929c"
#define TITAN_LIBS_KEY     "ecfc9174-d8fd-11e7-8be2-abbaa3ded45f"

#define TITAN_LUAINTEGER_NBITS cast_int(sizeof(lua_Integer) * CHAR_BIT)

/*
** Macro to convert pointer-to-void* to pointer-to-function. This cast
** is undefined according to ISO C, but POSIX assumes that it works.
** (The '__extension__' in gnu compilers is only to avoid warnings.)
*/
#if defined(__GNUC__)
#define cast_func(t,p) (__extension__ (t)(p))
#else
#define cast_func(t,p) ((t)(p))
#endif

#define MAXNUMBER2STR 50

inline static TString* _integer2str (lua_State *L, lua_Integer i) {
    size_t len;
    static char _cvtbuff[MAXNUMBER2STR];
    len = lua_integer2str(_cvtbuff, sizeof(_cvtbuff), i);
    return luaS_newlstr(L, _cvtbuff, len);
}

inline static TString* _float2str (lua_State *L, lua_Number f) {
    size_t len;
    static char _cvtbuff[MAXNUMBER2STR];
    len = lua_number2str(_cvtbuff, sizeof(_cvtbuff), f);
    return luaS_newlstr(L, _cvtbuff, len);
}

LUAI_FUNC void *loadlib (lua_State *L, const char *file);
LUAI_FUNC void *loadsym (lua_State *L, void *lib, const char *sym);
LUAI_FUNC const TValue *getgeneric (Table *t, const TValue *key);

#endif
