local typedecl = require 'titan-compiler.typedecl'

return typedecl("Ast", {
    Type = {
        TypeNil         = {"loc"},
        TypeBoolean     = {"loc"},
        TypeInteger     = {"loc"},
        TypeFloat       = {"loc"},
        TypeString      = {"loc"},
        TypeValue       = {"loc"},
        TypeQualName    = {"loc", "module", "name"},
        TypeName        = {"loc", "name"},
        TypeArray       = {"loc", "subtype"},
        TypeFunction    = {"loc", "argtypes", "rettypes"},
    },

    TopLevel = {
        TopLevelMethod  = {"loc", "class", "name", "params", "rettypes", "block"},
        TopLevelStatic  = {"loc", "class", "name", "params", "rettypes", "block"},
        TopLevelFunc    = {"loc", "islocal", "name", "params", "rettypes", "block"},
        TopLevelVar     = {"loc", "islocal", "decl", "value"},
        TopLevelRecord  = {"loc", "name", "fields"},
        TopLevelImport  = {"loc", "localname", "modname"},
        TopLevelForeignImport = {"loc", "localname", "headername"},
    },

    Decl = {
        Decl = {"loc", "name", "type"},
    },

    Stat = {
        StatBlock       = {"loc", "stats"},
        StatWhile       = {"loc", "condition", "block"},
        StatRepeat      = {"loc", "block", "condition"},
        StatIf          = {"loc", "thens", "elsestat"},
        StatFor         = {"loc", "decl", "start", "finish", "inc", "block"},
        StatAssign      = {"loc", "vars", "exps"},
        StatDecl        = {"loc", "decls", "exps"},
        StatCall        = {"loc", "callexp"},
        StatReturn      = {"loc", "exps"},
    },

    Then = {
        Then            = {"loc", "condition", "block"},
    },

    Var = {
        VarName         = {"loc", "name"},
        VarBracket      = {"loc", "exp1", "exp2"},
        VarDot          = {"loc", "exp", "name"}
    },

    Exp = {
        ExpNil          = {"loc"},
        ExpBool         = {"loc", "value"},
        ExpInteger      = {"loc", "value"},
        ExpFloat        = {"loc", "value"},
        ExpString       = {"loc", "value"},
        ExpInitList     = {"loc", "fields"},
        ExpCall         = {"loc", "exp", "args"},
        ExpVar          = {"loc", "var"},
        ExpUnop         = {"loc", "op", "exp"},
        ExpConcat       = {"loc", "exps"},
        ExpBinop        = {"loc", "lhs", "op", "rhs"},
        ExpCast         = {"loc", "exp", "target"},
        ExpAdjust       = {"loc", "exp"},                   -- adjust call to just first value
        ExpExtra        = {"loc", "exp", "index", "_type"}  -- index-th (>1) value of call
    },

    Args = {
        ArgsFunc        = {"loc", "args"},
        ArgsMethod      = {"loc", "method", "args"},
    },

    Field = {
        Field           = {"loc", "name", "exp"},
    },
})
