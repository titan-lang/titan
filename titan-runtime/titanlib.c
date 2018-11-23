#include "lua.h"
#include "lauxlib.h"

#define TITAN_BACKTRACE_KEY "57d80578-2534-4856-ad3b-bde8cba4c445"

static int getbacktrace(lua_State *L) {
  lua_pushliteral(L, TITAN_BACKTRACE_KEY);
  lua_rawget(L, LUA_REGISTRYINDEX);
  return 1;
}

static const luaL_Reg titanlib[] = {
  { "backtrace", getbacktrace },
  { NULL, NULL }
};

int luaopen_titan(lua_State *L) {
  luaL_newlib(L, titanlib);
  return 1;
}
