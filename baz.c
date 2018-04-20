#include <stdlib.h>
#include <string.h>
#include "luaconf.h"

#include "lauxlib.h"
#include "lualib.h"

#include "lapi.h"
#include "lgc.h"
#include "ltable.h"
#include "lfunc.h"
#include "lstring.h"
#include "lvm.h"

#include "lobject.h"

#include <math.h>

#ifdef __GNUC__
#define TITAN_LIKELY(x)   __builtin_expect((x), 1)
#define TITAN_UNLIKELY(x) __builtin_expect((x), 0)
#else
#define TITAN_LIKELY(x)   (x)
#define TITAN_UNLIKELY(x) (x)
#endif

#define TITAN_LUAINTEGER_NBITS cast_int(sizeof(lua_Integer) * CHAR_BIT)

#include <dlfcn.h>

#define TITAN_VER          "0.5"
#define TITAN_VER_SUFFIX   "_0_5"
#define TITAN_PATH_VAR     "TITAN_PATH"
#define TITAN_PATH_SEP     "/"
#define TITAN_PATH_DEFAULT ".;/usr/local/lib/titan/" TITAN_VER
#define TITAN_PATH_KEY     "ec10e486-d8fd-11e7-87f4-e7e9581a929c"
#define TITAN_LIBS_KEY     "ecfc9174-d8fd-11e7-8be2-abbaa3ded45f"

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

static void *loadlib (lua_State *L, const char *file) {
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

static void *loadsym (lua_State *L, void *lib, const char *sym) {
    void *f = dlsym(lib, sym);
    if(f == NULL) luaL_error(L, dlerror());
    return f;
}

#define MAXNUMBER2STR 50

#ifdef __clang__
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wparentheses-equality"
#endif

static char _cvtbuff[MAXNUMBER2STR];

inline static TString* _integer2str (lua_State *L, lua_Integer i) {
    size_t len;
    len = lua_integer2str(_cvtbuff, sizeof(_cvtbuff), i);
    return luaS_newlstr(L, _cvtbuff, len);
}

inline static TString* _float2str (lua_State *L, lua_Number f) {
    size_t len;
    len = lua_number2str(_cvtbuff, sizeof(_cvtbuff), f);
    return luaS_newlstr(L, _cvtbuff, len);
}

static ptrdiff_t foo_Point_typetag;
static Table *foo_Point_typemt;

CClosure* baz_point_titan(lua_State *L, lua_Number _param_x, lua_Number _param_y);

static int _initialized = 0;

CClosure* baz_point_titan(lua_State *L, lua_Number _param_x, lua_Number _param_y) {
    luaC_checkGC(L);
    /* function preamble: reserve needed stack space */
    if (L->stack_last - L->top > 3) {
        if (L->ci->top < L->top + 3) L->ci->top = L->top + 3;
    } else {
        lua_checkstack(L, 3);
    }
    TValue *_base = L->top;
    L->top += 3;
    for(TValue *_s = L->top - 1; _base <= _s; _s--) {
        setnilvalue(_s);
    }
    {
        TValue *_tmp_1_slot = _base + 0;
        CClosure* _tmp_1 = 0;
        _tmp_1 = luaF_newCclosure(L, 3);
        {
            Udata* _ud = luaS_newudata(L, sizeof(ptrdiff_t));
            *((ptrdiff_t*)(getudatamem(_ud))) = foo_Point_typetag;
            _ud->metatable = foo_Point_typemt;
            _ud->user_.gc = (GCObject*)_tmp_1;
            _ud->ttuv_ = ctb(LUA_TCCL);
            setuvalue(L, &(_tmp_1->upvalue[0]), _ud);
        }
        setclCvalue(L, _tmp_1_slot, _tmp_1);
        TValue *_tmp_2_slot = _base + 1;
        lua_Number _tmp_2 = 0;
        _tmp_2 = ((lua_Number)2);
        setfltvalue(_tmp_2_slot, _tmp_2);;
        setobj2t(L, &(_tmp_1->upvalue[1]), _tmp_2_slot);
        TValue *_tmp_3_slot = _base + 2;
        lua_Number _tmp_3 = 0;
        _tmp_3 = ((lua_Number)3);
        setfltvalue(_tmp_3_slot, _tmp_3);;
        setobj2t(L, &(_tmp_1->upvalue[2]), _tmp_3_slot);
        L->top = _base;
        return _tmp_1;
    }
}

static int baz_point_lua(lua_State *L) {
    TValue *func = L->ci->func;
    if((L->top - func - 1) != 2) {
        luaL_error(L, "calling Titan function %s with %d arguments, but expected %d", "point", L->top - func - 1, 2);
    }
    lua_Number _param_x = 0;
    if (TITAN_LIKELY(ttisfloat((func+ 1)))) {
        _param_x = fltvalue((func+ 1));
    } else if (ttisinteger((func+ 1))) {
        _param_x = (lua_Number)ivalue((func+ 1));
    } else {
        luaL_error(L, "type error at line %d, expected float but found %s", 2, lua_typename(L, ttnov((func+ 1))));
    }
    lua_Number _param_y = 0;
    if (TITAN_LIKELY(ttisfloat((func+ 2)))) {
        _param_y = fltvalue((func+ 2));
    } else if (ttisinteger((func+ 2))) {
        _param_y = (lua_Number)ivalue((func+ 2));
    } else {
        luaL_error(L, "type error at line %d, expected float but found %s", 2, lua_typename(L, ttnov((func+ 2))));
    }
    lua_checkstack(L, 1);
    TValue *_firstret = L->top;
    L->top += 1;
    CClosure* res = baz_point_titan(L, _param_x, _param_y);
    setobj2t(L, _firstret, &((res)->upvalue[0]));
    ;
    _firstret++;
    return 1;
}

int baz_types(lua_State* L) {
    lua_pushliteral(L, "Module('baz',{point = ModuleMember('baz', 'point', Function({Float(),Float()},{Nominal('foo.Point')},false))})");
    return 1;
}

void baz_init(lua_State *L) {
    if(!_initialized) {
        _initialized = 1;
        void *foo_handle = loadlib(L, "foo.so");
        void (*foo_init)(lua_State *L) = cast_func(void (*)(lua_State*), loadsym(L, foo_handle, "foo_init"));
        foo_init(L);
        foo_Point_typetag = ((ptrdiff_t)(loadsym(L, foo_handle, "foo_Point_typetag")));
        foo_Point_typemt = *((Table**)(loadsym(L, foo_handle, "foo_Point_typemt")));
    }
}

int luaopen_baz(lua_State *L) {
    baz_init(L);
    lua_newtable(L);
    lua_pushcfunction(L, baz_point_lua);
    lua_setfield(L, -2, "point");
    luaL_setmetatable(L, "titan module baz");
    return 1;
}

