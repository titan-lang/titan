#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <dlfcn.h>
#include "lua.h"
#include "lauxlib.h"
#include "ltable.h"
#include "lobject.h"
#include "llimits.h"
#include "lvm.h"
#include "titan.h"

static void pushpath (lua_State *L) {
    lua_pushliteral(L, TITAN_PATH_KEY);
    lua_rawget(L, LUA_REGISTRYINDEX);
    if(lua_isnil(L, -1)) {
        lua_pop(L, 1);

        /* Try the versioned name for the Titan Path variable */
        const char *path = getenv(TITAN_PATH_VAR TITAN_VER_SUFFIX);
        if (path == NULL) {
            /* Try the unversioned name for the Titan Path variable */
            path = getenv(TITAN_PATH_VAR);
        }
        if (path == NULL) {
            /* No Titan Path environment variable */
            path = TITAN_PATH_DEFAULT;
            lua_pushstring(L, path);
        } else {
            path = luaL_gsub(L, path, ";;", ";\1;");
            path = luaL_gsub(L, path, "\1", TITAN_PATH_DEFAULT);
            lua_remove(L, -2); /* remove result from 1st 'gsub' */
        }
        lua_pushliteral(L, TITAN_PATH_KEY);
        lua_pushvalue(L, -2);
        lua_rawset(L, LUA_REGISTRYINDEX);
    }
}

static const char *pushnextdir (lua_State *L, const char *path) {
    const char *l;
    while (*path == ';') path++;  /* skip separators */
    if (*path == '\0') return NULL;  /* no more templates */
    l = strchr(path, ';');  /* find next separator */
    if (l == NULL) l = path + strlen(path);
    lua_pushlstring(L, path, l - path);  /* template */
    return l;
}

static int gctm (lua_State *L) {
    lua_Integer n = luaL_len(L, 1);
    /* for each handle, in reverse order */
    for (; n >= 1; n--) {
        lua_rawgeti(L, 1, n);  /* get handle LIBS[n] */
        dlclose(lua_touserdata(L, -1));
        lua_pop(L, 1);  /* pop handle */
    }
    return 0;
}

static void createlibstable (lua_State *L) {
    lua_newtable(L);
    lua_createtable(L, 0, 1);  /* create metatable */
    lua_pushcfunction(L, gctm);
    lua_setfield(L, -2, "__gc");  /* set finalizer */
    lua_setmetatable(L, -2);
    lua_pushliteral(L, TITAN_LIBS_KEY);
    lua_pushvalue(L, -2);
    lua_rawset(L, LUA_REGISTRYINDEX);
}

static void pushlibs(lua_State *L) {
    lua_pushliteral(L, TITAN_LIBS_KEY);
    lua_rawget(L, LUA_REGISTRYINDEX);
    if(lua_isnil(L, -1)) {
        lua_pop(L, 1);
        createlibstable(L);
    }
}

void *loadlib (lua_State *L, const char *file) {
    pushlibs(L);
    lua_pushstring(L, file);
    lua_rawget(L, -2); // try to get lib
    if(!lua_isnil(L, -1)) {
        void *lib = lua_touserdata(L, -1);
        lua_pop(L, 2); // pop lib and libs table
        return lib;
    } else {
        lua_pop(L, 1); // pop nil
        pushpath(L);
        const char *path = lua_tostring(L, -1);
        while((path = pushnextdir(L, path)) != NULL) {
            const char *dir = lua_tostring(L, -1);
            lua_pushfstring(L, "%s" TITAN_PATH_SEP "%s", dir, file);
            const char *filename = lua_tostring(L, -1);
            void *lib = dlopen(filename, RTLD_NOW | RTLD_LOCAL);
            if(lib != NULL) {
                lua_pop(L, 3); // pop path, filename, and dir
                lua_pushstring(L, file);
                lua_pushlightuserdata(L, lib);
                lua_rawset(L, -3); // add to libs table
                lua_pop(L, 1); // pop libs table
                return lib;
            }
            lua_pop(L, 2); // pop filename and dir
        }
        lua_pop(L, 2); // pop path and libs table
        luaL_error(L, dlerror());
        return NULL;
    }
}

void *loadsym (lua_State *L, void *lib, const char *sym) {
    void *f = dlsym(lib, sym);
    if(f == NULL) luaL_error(L, dlerror());
    return f;
}

#define hashpow2(t,n)           (gnode(t, lmod((n), sizenode(t))))

#define hashstr(t,str)          hashpow2(t, (str)->hash)
#define hashboolean(t,p)        hashpow2(t, p)
#define hashint(t,i)            hashpow2(t, i)


/*
** for some types, it is better to avoid modulus by power of 2, as
** they tend to have many 2 factors.
*/
#define hashmod(t,n)    (gnode(t, ((n) % ((sizenode(t)-1)|1))))


#define hashpointer(t,p)        hashmod(t, point2uint(p))

/*
** Hash for floating-point numbers.
** The main computation should be just
**     n = frexp(n, &i); return (n * INT_MAX) + i
** but there are some numerical subtleties.
** In a two-complement representation, INT_MAX does not has an exact
** representation as a float, but INT_MIN does; because the absolute
** value of 'frexp' is smaller than 1 (unless 'n' is inf/NaN), the
** absolute value of the product 'frexp * -INT_MIN' is smaller or equal
** to INT_MAX. Next, the use of 'unsigned int' avoids overflows when
** adding 'i'; the use of '~u' (instead of '-u') avoids problems with
** INT_MIN.
*/
#if !defined(l_hashfloat)
static int l_hashfloat (lua_Number n) {
  int i;
  lua_Integer ni;
  n = l_mathop(frexp)(n, &i) * -cast_num(INT_MIN);
  if (!lua_numbertointeger(n, &ni)) {  /* is 'n' inf/-inf/NaN? */
    lua_assert(luai_numisnan(n) || l_mathop(fabs)(n) == cast_num(HUGE_VAL));
    return 0;
  }
  else {  /* normal case */
    unsigned int u = cast(unsigned int, i) + cast(unsigned int, ni);
    return cast_int(u <= cast(unsigned int, INT_MAX) ? u : ~u);
  }
}
#endif
/*
** returns the 'main' position of an element in a table (that is, the index
** of its hash value)
*/
static Node *mainposition (const Table *t, const TValue *key) {
  switch (ttype(key)) {
    case LUA_TNUMINT:
      return hashint(t, ivalue(key));
    case LUA_TNUMFLT:
      return hashmod(t, l_hashfloat(fltvalue(key)));
    case LUA_TSHRSTR:
      return hashstr(t, tsvalue(key));
    case LUA_TLNGSTR:
      return hashpow2(t, luaS_hashlongstr(tsvalue(key)));
    case LUA_TBOOLEAN:
      return hashboolean(t, bvalue(key));
    case LUA_TLIGHTUSERDATA:
      return hashpointer(t, pvalue(key));
    case LUA_TLCF:
      return hashpointer(t, fvalue(key));
    default:
      lua_assert(!ttisdeadkey(key));
      return hashpointer(t, gcvalue(key));
  }
}

/*
** "Generic" get version. (Not that generic: not valid for integers,
** which may be in array part, nor for floats with integral values.)
*/
const TValue *getgeneric (Table *t, const TValue *key) {
  Node *n = mainposition(t, key);
  for (;;) {  /* check whether 'key' is somewhere in the chain */
    if (luaV_rawequalobj(gkey(n), key))
      return gval(n);  /* that's it */
    else {
      int nx = gnext(n);
      if (nx == 0)
        return luaO_nilobject;  /* not found */
      n += nx;
    }
  }
}

/* Builtin foreign functions */
int titan_print(lua_State *L, CClosure *_mod, int nargs, ...) {
    if(nargs > 0) {
        lua_checkstack(L, 3);
        TValue *top = L->top;
        va_list args;
        va_start(args, nargs);
        for(int i = 0; i < nargs; i++) {
            TValue val = va_arg(args, TValue);
            setobj2s(L, L->top-1, &val);
            size_t l;
            const char *s = luaL_tolstring(L, -1, &l);
            if(i > 0) lua_writestring("\t", 1);
            lua_writestring(s, l);
            L->top--;
        }
        va_end(args);
        L->top = top;
    }
    lua_writeline();
    return 0;
}

TValue titan_assert(lua_State *L, CClosure *_mod, TValue cond, TString *msg) {
    if(l_isfalse(&cond)) {
        luaL_error(L, getstr(msg));
    }
    return cond;
}

Table *titan_dofile(lua_State *L, CClosure *_mod, TString *fname, int nargs, ...) {
    TValue *top = L->top;
    lua_checkstack(L, 2 + nargs);
    lua_createtable(L, 0, 0);
    Table *tres = hvalue(L->top-1);
    TValue *firstres = L->top;
    if (luaL_loadfile(L, getstr(fname)) != LUA_OK) {
        lua_error(L);
    }
    if(nargs > 0) {
        va_list args;
        va_start(args, nargs);
        for(int i = 0; i < nargs; i++) {
            TValue arg = va_arg(args, TValue);
            L->top++;
            setobj2s(L, L->top-1, &arg);
        }
        va_end(args);        
    }
    lua_call(L, nargs, LUA_MULTRET);
    for(TValue *res = firstres; res < L->top; res++) {
        luaH_setint(L, tres, res - firstres + 1, res);
    }
    L->top = top;
    return tres;
}

int titan_error(lua_State *L, CClosure *_mod, TString *msg) {
    return luaL_error(L, getstr(msg));
}

Table *titan_dostring(lua_State *L, CClosure *_mod, TString *code, int nargs, ...) {
    TValue *top = L->top;
    lua_checkstack(L, 2 + nargs);
    lua_createtable(L, 0, 0);
    Table *tres = hvalue(L->top-1);
    TValue *firstres = L->top;
    if (luaL_loadstring(L, getstr(code)) != LUA_OK) {
        lua_error(L);
    }
    if(nargs > 0) {
        va_list args;
        va_start(args, nargs);
        for(int i = 0; i < nargs; i++) {
            TValue arg = va_arg(args, TValue);
            L->top++;
            setobj2s(L, L->top-1, &arg);
        }
        va_end(args);        
    }
    lua_call(L, nargs, LUA_MULTRET);
    for(TValue *res = firstres; res < L->top; res++) {
        luaH_setint(L, tres, res - firstres + 1, res);
    }
    L->top = top;
    return tres;
}

TString *titan_tostring(lua_State *L, CClosure *_mod, TValue val) {
    TValue *top = L->top;
    lua_checkstack(L, 3);
    L->top++;
    setobj2s(L, L->top-1, &val);
    size_t l;
    luaL_tolstring(L, -1, &l);
    TString *s = tsvalue(L->top-1);
    L->top = top;
    return s;
}

lua_Number titan_tofloat(lua_State *L, CClosure *_mod, TString *s) {
    TValue val;
    if(luaO_str2num(getstr(s), &val)) {
        return nvalue(&val);
    } else {
        return 0;
    }
}

lua_Integer titan_tointeger(lua_State *L, CClosure *_mod, TString *s) {
    TValue val;
    if(luaO_str2num(getstr(s), &val)) {
        lua_Integer res;
        if(luaV_tointeger(&val, &res, 0)) {
            return res;
        } else {
            return 0;
        }
    } else {
        return 0;
    }
}

lua_Integer titan_string_byte(lua_State *L, CClosure *_mod, TString *s, lua_Integer index) {
    size_t len = tsslen(s);
    if(index == 0)
        return 0;
    else if(index < 0) 
        index = index % len; 
    else 
        index = (index - 1) % len;
    return getstr(s)[index];
}
