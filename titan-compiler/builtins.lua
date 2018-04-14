local ast = require "titan-compiler.ast"

return {
    table_remove = ast.Toplevel.Builtin(false, "table.remove"),
}
