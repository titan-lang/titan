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

lua_Number test_Point_getx_titanmethod(lua_State *L, CClosure* __self__);

lua_Number test_Point_gety_titanmethod(lua_State *L, CClosure* __self__);

int test_Point_move_titanmethod(lua_State *L, CClosure* __self__, lua_Number _param_dx, lua_Number _param_dy);

lua_Number test_f_titan(lua_State *L, lua_Number *_outparam_2);

static int _initialized = 0;

int test_Point_typetag;

Table *test_Point_typemt;

lua_Number test_Point_getx_titanmethod(lua_State *L, CClosure* __self__) {
    {
        return  fltvalue(&((__self__)->upvalue[1]));
    }
}

lua_Number test_Point_gety_titanmethod(lua_State *L, CClosure* __self__) {
    {
        return  fltvalue(&((__self__)->upvalue[2]));
    }
}

int test_Point_move_titanmethod(lua_State *L, CClosure* __self__, lua_Number _param_dx, lua_Number _param_dy) {
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

lua_Number test_f_titan(lua_State *L, lua_Number *_outparam_2) {
    luaC_checkGC(L);
    /* function preamble: reserve needed stack space */
    if (L->stack_last - L->top > 4) {
        if (L->ci->top < L->top + 4) L->ci->top = L->top + 4;
    } else {
        lua_checkstack(L, 4);
    }
    TValue *_base = L->top;
    L->top += 4;
    for(TValue *_s = L->top - 1; _base <= _s; _s--) {
        setnilvalue(_s);
    }
    {
        CClosure* _local_r_1;
        TValue *_localslot_r_2 = _base + 3;
        TValue *_tmp_1_slot = _base + 0;
        CClosure* _tmp_1 = 0;
        _tmp_1 = luaF_newCclosure(L, 3);
        {
            Udata* _ud = luaS_newudata(L, sizeof(ptrdiff_t));
            *((ptrdiff_t*)(getudatamem(_ud))) = ((ptrdiff_t)&test_Point_typetag);
            _ud->metatable = test_Point_typemt;
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
        _local_r_1 = _tmp_1;
        /* update slot */
        setclCvalue(L, _localslot_r_2, _local_r_1);
        test_Point_move_titanmethod(L, _local_r_1, ((lua_Number)2), ((lua_Number)3));
        *_outparam_2 = test_Point_gety_titanmethod(L, _local_r_1);
        L->top = _base;
        return test_Point_getx_titanmethod(L, _local_r_1);
    }
}

static int test_f_lua(lua_State *L) {
    TValue *func = L->ci->func;
    if((L->top - func - 1) != 0) {
        luaL_error(L, "calling Titan function %s with %d arguments, but expected %d", "f", L->top - func - 1, 0);
    }
    lua_Number _outparam_2 = 0;
    lua_checkstack(L, 2);
    TValue *_firstret = L->top;
    L->top += 2;
    lua_Number res = test_f_titan(L, &_outparam_2);
    setfltvalue(_firstret, res);;
    _firstret++;
    setfltvalue(_firstret, _outparam_2);;
    _firstret++;
    return 2;
}

int test_types(lua_State* L) {
    lua_pushliteral(L, "Module('test',{f = ModuleMember('test', 'f', Function({},{Float(),Float()},false)),Point = ModuleMember('test', 'Point', Record('test.Point',{Field('test.Point', 'x', Float(), 1),Field('test.Point', 'y', Float(), 2)},{new = StaticMethod('test.Point', 'new',{Float(),Float()},{Nominal('test.Point')})},{gety = Method('test.Point', 'gety',{},{Float()}),getx = Method('test.Point', 'getx',{},{Float()}),move = Method('test.Point', 'move',{Float(),Float()},{Nil()})}, {}))})");
    return 1;
}

void test_init(lua_State *L) {
    if(!_initialized) {
        _initialized = 1;
        luaL_newmetatable(L, "Titan record test.Point"); /* push metatable */
        test_Point_typemt = hvalue(L->top);
        L->top--;
        lua_pushlightuserdata(L, &test_Point_typetag);
        lua_pushstring(L, "test.Point");
        lua_rawset(L, LUA_REGISTRYINDEX);
    }
}

int luaopen_test(lua_State *L) {
    test_init(L);
    lua_newtable(L);
    lua_pushcfunction(L, test_f_lua);
    lua_setfield(L, -2, "f");
    luaL_setmetatable(L, "titan module test");
    return 1;
}

