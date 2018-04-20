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

int foo_Point_move_titanmethod(lua_State *L, CClosure* __self__, lua_Number _param_dx, lua_Number _param_dy);

static int _initialized = 0;

int foo_Point_typetag;

Table *foo_Point_typemt;

int foo_Point_move_titanmethod(lua_State *L, CClosure* __self__, lua_Number _param_dx, lua_Number _param_dy) {
    {
        lua_Number _tmp_1 = 0;
        _tmp_1 = ( fltvalue(&((__self__)->upvalue[1]))+_param_dx);
        lua_Number _tmp_2 = 0;
        _tmp_2 = ( fltvalue(&((__self__)->upvalue[2]))+_param_dy);
        setfltvalue(&((__self__)->upvalue[1]), _tmp_1);
        setfltvalue(&((__self__)->upvalue[2]), _tmp_2);
    }
    return 0;
}

int foo_types(lua_State* L) {
    lua_pushliteral(L, "Module('foo',{Point = ModuleMember('foo', 'Point', Record('foo.Point',{Field('foo.Point', 'x', Float(), 1),Field('foo.Point', 'y', Float(), 2)},{new = StaticMethod('foo.Point', 'new',{Float(),Float()},{Nominal('foo.Point')})},{move = Method('foo.Point', 'move',{Float(),Float()},{Nil()})}, {}))})");
    return 1;
}

void foo_init(lua_State *L) {
    if(!_initialized) {
        _initialized = 1;
        luaL_newmetatable(L, "Titan record foo.Point"); /* push metatable */
        foo_Point_typemt = hvalue(L->top);
        L->top--;
        lua_pushlightuserdata(L, &foo_Point_typetag);
        lua_pushstring(L, "foo.Point");
        lua_rawset(L, LUA_REGISTRYINDEX);
    }
}

int luaopen_foo(lua_State *L) {
    foo_init(L);
    lua_newtable(L);
    luaL_setmetatable(L, "titan module foo");
    return 1;
}

