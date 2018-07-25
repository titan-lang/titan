local parser = {}

local re = require "relabel"
local inspect = require "inspect"

local ast = require "titan-compiler.ast"
local lexer = require "titan-compiler.lexer"
local location = require "titan-compiler.location"
local syntax_errors = require "titan-compiler.syntax_errors"

-- File name of the file that is currently being parsed.
-- Since this is a global the parser is not reentrant but we couldn't think of
-- a better way yet. (If only lpeg.re had Carg...)
local THIS_FILENAME = nil

--
-- Functions used by the PEG grammar
--

local defs = {}
local synerr 

for tokname, tokpat in pairs(lexer) do
    defs[tokname] = tokpat
end

for tag, cons in pairs(ast) do
    defs[tag] = cons
end

defs['adderror'] = function (pos, lab)
	table.insert(synerr, { pos = pos, lab = lab })
	return
end

defs['defaultInt'] = 52
defs['defaultInt2'] = function () return 52 end
defs['defaultMethName'] = 'm42'
defs['defaultQualName'] = 'q42'
defs['defaultFuncName'] = 'f42'
defs['defaultRecName'] = 'rec42'
defs['defaultFieldRec'] = function() return 'field42'  end
defs['defaultImportName'] = 'imp42'
defs['defaultStringImportName'] = 'mod42'
defs['defaultForeignName'] = 'foreign42'
defs['defaultDeclName'] = 'dec42'
defs['defaultColonName'] = 'col42'
defs['defaultDotName'] = 'dot42'

function defs.get_loc(s, pos)
    return true, location.from_pos(THIS_FILENAME, s, pos)
end

function defs.totrue()
    return true
end

function defs.tofalse()
    return false
end

function defs.rettypeopt(pos, x)
    if not x then
        -- When possible, we should change this default to the empty list
        -- or infer the return type.
        return { ast.TypeNil(pos) }
    else
        return x
    end
end

function defs.opt(x)
    if x == "" then
        return false
    else
        return x
    end
end

function defs.listopt(x)
    if x == "" then
        return {}
    else
        return x
    end
end

function defs.boolopt(x)
    return x ~= ""
end

function defs.nil_exp(pos--[[, s ]])
    -- We can't call ast.ExpNil directly in the parser because we
    -- need to drop the string capture that comes in the second argument.
    return ast.ExpNil(pos)
end

function defs.number_exp(pos, n)
    --print("number_exp", pos, n, math.type(n))
    --assert(n ~= 52)
    --assert(n ~= '52')
    if math.type(n) == "integer" then
        return ast.ExpInteger(pos, n)
    elseif math.type(n) == "float" then
        return ast.ExpFloat(pos, n)
    else
        error("impossible")
    end
end

function defs.name_exp(pos, name)
    return ast.ExpVar(pos, ast.VarName(pos, name))
end

function defs.adjust_exp(pos, exp)
    if exp._tag == "Ast.ExpCall" then
        return ast.ExpAdjust(pos, exp)
    else
        return exp
    end
end

function defs.ifstat(pos, exp, block, thens, elseopt)
    table.insert(thens, 1, ast.Then(pos, exp, block))
    return ast.StatIf(pos, thens, elseopt)
end

function defs.fold_binop_left(pos, matches)
    local lhs = matches[1]
    for i = 2, #matches, 2 do
        local op  = matches[i]
        local rhs = matches[i+1]
        lhs = ast.ExpBinop(pos, lhs, op, rhs)
    end
    return lhs
end

-- Should this go on a separate constant propagation pass?
function defs.binop_concat(pos, lhs, op, rhs)
    if op then
        if rhs._tag == "Ast.ExpConcat" then
            table.insert(rhs.exps, 1, lhs)
            return rhs
        elseif (lhs._tag == "Ast.ExpString" or
            lhs._tag == "Ast.ExpInteger" or
            lhs._tag == "Ast.ExpFloat") and
            (rhs._tag == "Ast.ExpString" or
            rhs._tag == "Ast.ExpInteger" or
            rhs._tag == "Ast.ExpFloat") then
            return ast.ExpString(pos, lhs.value .. rhs.value)
        else
            return ast.ExpConcat(pos, { lhs, rhs })
        end
    else
        return lhs
    end
end

function defs.binop_right(pos, lhs, op, rhs)
    if op then
        return ast.ExpBinop(pos, lhs, op, rhs)
    else
        return lhs
    end
end

function defs.fold_unops(pos, unops, exp)
    for i = #unops, 1, -1 do
        local op = unops[i]
        exp = ast.ExpUnop(pos, op, exp)
    end
    return exp
end

-- We represent the suffix of an expression by a function that receives the
-- base expression and returns a full expression including the suffix.

function defs.suffix_funccall(pos, args)
    return function(exp)
        return ast.ExpCall(pos, exp, ast.ArgsFunc(pos, args))
    end
end

function defs.suffix_methodcall(pos, name, args)
    return function(exp)
        return ast.ExpCall(pos, exp, ast.ArgsMethod(pos, name, args))
    end
end

function defs.suffix_bracket(pos, index)
    return function(exp)
        return ast.ExpVar(pos, ast.VarBracket(pos, exp, index))
    end
end

function defs.suffix_dot(pos, name)
    return function(exp)
        return ast.ExpVar(pos, ast.VarDot(pos, exp, name))
    end
end

function defs.fold_suffixes(exp, suffixes)
    for i = 1, #suffixes do
        local suf = suffixes[i]
        exp = suf(exp)
    end
    return exp
end

function defs.exp2var(exp)
    return exp.var
end

function defs.exp_is_var(_, pos, exp)
    if exp._tag == "Ast.ExpVar" then
        return pos, exp
    else
        return false
    end
end

function defs.exp_is_call(_, pos, exp)
    if exp._tag == "Ast.ExpCall" then
        return pos, exp
    else
        return false
    end
end

function defs.recorddecl(pos, name, fields)
    local params, initlist = {}, {}
    for _, field in ipairs(fields) do
        table.insert(params, ast.Decl(field.loc, field.name, field.type))
        table.insert(initlist, ast.Field(field.loc, field.name,
            ast.ExpVar(field.loc, ast.VarName(field.loc, field.name))))
    end
    local body = ast.StatBlock(pos, {
        ast.StatReturn(pos, {
            ast.ExpInitList(pos, initlist)
        })
    })
    return ast.TopLevelRecord(pos, name, fields),
           ast.TopLevelStatic(pos, name, "new", params, { ast.TypeName(pos, name) }, body)
end

local grammar = re.compile([[

    program         <-  SKIP*
                        {| ( method
                           / toplevelfunc
                           / toplevelvar
                           / toplevelrecord
                           / import
                           / foreign )* |} (!. / %{EndInput})

    method          <- (P  FUNCTION NAME COLON NAME^NameMethod
                           LPAREN^LParPList paramlist RPAREN^RParPList
                           rettypeopt block END^EndFunc)         -> TopLevelMethod

    toplevelfunc    <- (P  localopt FUNCTION NAME^NameFunc
                           LPAREN^LParPList paramlist RPAREN^RParPList
                           rettypeopt block END^EndFunc)         -> TopLevelFunc

    toplevelvar     <- (P localopt decl ASSIGN^AssignVar
                           !(IMPORT / FOREIGN)
                           exp^ExpVarDec)                        -> TopLevelVar

    toplevelrecord  <- (P  RECORD NAME^NameRecord
                           recordfields^FieldRecord
                           END^EndRecord)                        -> recorddecl

    localopt        <- (LOCAL)?                                  -> boolopt

    import          <- (P  LOCAL NAME^NameImport ASSIGN^AssignImport
                          !FOREIGN IMPORT^ImportImport
                          (LPAREN STRINGLIT^StringLParImport RPAREN^RParImport /
                          STRINGLIT^StringImport))               -> TopLevelImport

    foreign         <- (P  LOCAL NAME^NameImport ASSIGN^AssignImport
                           FOREIGN IMPORT^ImportImport
                          (LPAREN STRINGLIT^StringLParImport RPAREN^RParImport /
                           STRINGLIT^StringImport))              -> TopLevelForeignImport

    rettypeopt      <- (P  (COLON rettype^TypeFunc)?)            -> rettypeopt

    paramlist       <- {| (param (COMMA param^ParamList)*)? |} -- produces {Decl}

    param           <- (P  NAME COLON^ParamSemicolon
                           type^TypeParam)                        -> Decl

    decl            <- (P  NAME (COLON type^TypeDecl)? -> opt)   -> Decl

    decllist        <- {| decl (COMMA decl^DeclParList)* |}      -- produces {Decl}

    simpletype      <- (P  NIL)                                  -> TypeNil
                     / (P  BOOLEAN)                              -> TypeBoolean
                     / (P  INTEGER)                              -> TypeInteger
                     / (P  FLOAT)                                -> TypeFloat
                     / (P  STRING)                               -> TypeString
                     / (P  VALUE)                                -> TypeValue
                     / (P  NAME DOT NAME^QualName)               -> TypeQualName
                     / (P  NAME)                                 -> TypeName
                     / (P  LCURLY type^TypeType
                           COLON
                           type^TypeType
                           RCURLY^RCurlyType)                    -> TypeMap
                     / (P  LCURLY type^TypeType
                           RCURLY^RCurlyType)                    -> TypeArray

    typelist        <- ( LPAREN
                         {| (type (COMMA type^TypelistType)*)? |}
                         RPAREN^RParenTypelist )                 -- produces {Type}

    rettype         <- {| (P  typelist RARROW
                            rettype^TypeReturnTypes)             -> TypeFunction |}
                     / {| (P  {| simpletype |} RARROW
                             rettype^TypeReturnTypes)            -> TypeFunction |}
                     / typelist
                     / {| simpletype |}

    type            <- (P  typelist RARROW^Err_037
                           rettype^TypeReturnTypes)              -> TypeFunction
                     / (P  {| simpletype |} RARROW
                           rettype^TypeReturnTypes)              -> TypeFunction
                     / simpletype

    recordfields    <- {| recordfield* |}                        -- produces {Decl}

    recordfield     <- (P  NAME COLON^ColonRecordField
                           type^TypeRecordField SEMICOLON?)      -> Decl

    block           <- (P  {| statement* returnstat? |})         -> StatBlock

    statement       <- (SEMICOLON)                               -- ignore
                     / (DO block END^EndBlock)                   -- produces StatBlock
                     / (P  WHILE exp^ExpWhile DO^DoWhile
                                 block END^EndWhile)             -> StatWhile
                     / (P  REPEAT block UNTIL^UntilRepeat
                                      exp^ExpRepeat)             -> StatRepeat
                     / (P  IF exp^ExpIf THEN^ThenIf block
                           elseifstats elseopt END^EndIf)        -> ifstat
                     / (P  FOR decl^DeclFor
                           ASSIGN^AssignFor exp^Exp1For
                           COMMA^CommaFor exp^Exp2For
                           (COMMA exp^Exp3For)?                  -> opt
                           DO^DoFor block END^EndFor)            -> StatFor
                     / (P  LOCAL decllist^DeclLocal ASSIGN^AssignLocal
                                 explist^ExpLocal)                   -> StatDecl
                     / (P  varlist ASSIGN^AssignAssign
                               explist^ExpAssign)                    -> StatAssign
                     / &(exp ASSIGN) %{ExpAssignPred}
                     / (P  (suffixedexp => exp_is_call))         -> StatCall
                     / &exp %{ExpStat}

    elseifstats     <- {| elseifstat* |}                         -- produces {Then}

    elseifstat      <- (P  ELSEIF exp^ExpElseIf
                           THEN^ThenElseIf block)                -> Then

    elseopt         <- (ELSE block)?                             -> opt

    returnstat      <- (P  RETURN (explist? -> listopt) SEMICOLON?)      -> StatReturn

    op1             <- ( OR -> 'or' )
    op2             <- ( AND -> 'and' )
    op3             <- ( EQ -> '==' / NE -> '~=' / LT -> '<' /
                         GT -> '>'  / LE -> '<=' / GE -> '>=' )
    op4             <- ( BOR -> '|' )
    op5             <- ( BXOR -> '~' )
    op6             <- ( BAND -> '&' )
    op7             <- ( SHL -> '<<' / SHR -> '>>' )
    op8             <- ( CONCAT -> '..' )
    op9             <- ( ADD -> '+' / SUB -> '-' )
    op10            <- ( MUL -> '*' / MOD -> '%%' / DIV -> '/' / IDIV -> '//' )
    unop            <- ( NOT -> 'not' / LEN -> '#' / NEG -> '-' / BNEG -> '~' )
    op12            <- ( POW -> '^' )

    exp             <- e1
    e1              <- (P  {| e2  (op1  e2^OpExp)* |})           -> fold_binop_left
    e2              <- (P  {| e3  (op2  e3^OpExp)* |})           -> fold_binop_left
    e3              <- (P  {| e4  (op3  e4^OpExp)* |})           -> fold_binop_left
    e4              <- (P  {| e5  (op4  e5^OpExp)* |})           -> fold_binop_left
    e5              <- (P  {| e6  (op5  e6^OpExp)* |})           -> fold_binop_left
    e6              <- (P  {| e7  (op6  e7^OpExp)* |})           -> fold_binop_left
    e7              <- (P  {| e8  (op7  e8^OpExp)* |})          -> fold_binop_left
    e8              <- (P     e9  (op8  e8^OpExp)?)             -> binop_concat
    e9              <- (P  {| e10 (op9  e10^OpExp)* |})           -> fold_binop_left
    e10             <- (P  {| e11 (op10 e11^OpExp)* |})          -> fold_binop_left
    e11             <- (P  {| unop* |}  e12)                     -> fold_unops
    e12             <- (P  castexp (op12 e11^OpExp)?)        -> binop_right

    suffixedexp     <- (prefixexp {| expsuffix+ |})              -> fold_suffixes

    expsuffix       <- (P  funcargs)                             -> suffix_funccall
                     / (P  COLON NAME^NameColonExpSuf
                                 funcargs^FuncArgsExpSuf)        -> suffix_methodcall
                     / (P  LBRACKET exp^ExpExpSuf
                                RBRACKET^RBracketExpSuf)         -> suffix_bracket
                     / (P  DOT NAME^NameDotExpSuf)               -> suffix_dot

    prefixexp       <- (P  NAME)                                 -> name_exp
                     / (P  LPAREN exp^ExpSimpleExp
                               RPAREN^RParSimpleExp)             -> adjust_exp


    castexp         <- (P  simpleexp AS type^CastMissingType)    -> ExpCast
                     / simpleexp                                 -- produces Exp

    simpleexp       <- (P  NIL)                                  -> nil_exp
                     / (P  FALSE -> tofalse)                     -> ExpBool
                     / (P  TRUE -> totrue)                       -> ExpBool
                     / (P  NUMBER)                               -> number_exp
                     / (P  STRINGLIT)                            -> ExpString
                     / initlist                                  -- produces Exp
                     / suffixedexp                               -- produces Exp
                     / prefixexp                                 -- produces Exp

    var             <- (suffixedexp => exp_is_var)               -> exp2var
                     / (P  NAME !expsuffix)                      -> name_exp -> exp2var

    varlist         <- {| var (COMMA var^ExpVarList)* |}            -- produces {Var}

    funcargs        <- (LPAREN (explist? -> listopt) RPAREN^RParFuncArgs)      -- produces {Exp}
                     / {| initlist |}                            -- produces {Exp}
                     / {| (P  STRINGLIT) -> ExpString |}         -- produces {Exp}

    explist         <- {| exp (COMMA exp^ExpExpList)* |}      -- produces {Exp}

    initlist        <- (P  LCURLY {| fieldlist? |}
                                  RCURLY^RCurlyInitList)         -> ExpInitList

    fieldlist       <- (field
                        (fieldsep
                         (field /
                          !RCURLY %{ExpFieldList}))*
                        fieldsep?)                          -- produces Field...

    field           <- (P  (key ASSIGN)? -> opt exp)       -> Field

    key             <- NAME
                     / LBRACKET exp^ExpExpSuf
                                RBRACKET^RBracketExpKey

    fieldsep        <- SEMICOLON / COMMA

    --
    -- Get current position
    --

    P <- {} => get_loc

    -- Create new rules for all our tokens, for the whitespace-skipping magic
    -- Currently done by hand but this is something that parser-gen should be
    -- able to do for us.

    SKIP            <- (%SPACE / %COMMENT)

    AND             <- %AND SKIP*
    BREAK           <- %BREAK SKIP*
    DO              <- %DO SKIP*
    ELSE            <- %ELSE SKIP*
    ELSEIF          <- %ELSEIF SKIP*
    END             <- %END SKIP*
    FALSE           <- %FALSE SKIP*
    FOR             <- %FOR SKIP*
    FUNCTION        <- %FUNCTION SKIP*
    GOTO            <- %GOTO SKIP*
    IF              <- %IF SKIP*
    IN              <- %IN SKIP*
    LOCAL           <- %LOCAL SKIP*
    NIL             <- %NIL SKIP*
    NOT             <- %NOT SKIP*
    OR              <- %OR SKIP*
    RECORD          <- %RECORD SKIP*
    REPEAT          <- %REPEAT SKIP*
    RETURN          <- %RETURN SKIP*
    THEN            <- %THEN SKIP*
    TRUE            <- %TRUE SKIP*
    UNTIL           <- %UNTIL SKIP*
    WHILE           <- %WHILE SKIP*
    IMPORT          <- %IMPORT SKIP*
    AS              <- %AS SKIP*
    FOREIGN         <- %FOREIGN SKIP*

    BOOLEAN         <- %BOOLEAN SKIP*
    INTEGER         <- %INTEGER SKIP*
    FLOAT           <- %FLOAT SKIP*
    STRING          <- %STRING SKIP*
    VALUE           <- %VALUE SKIP*

    ADD             <- %ADD SKIP*
    SUB             <- %SUB SKIP*
    MUL             <- %MUL SKIP*
    MOD             <- %MOD SKIP*
    DIV             <- %DIV SKIP*
    IDIV            <- %IDIV SKIP*
    POW             <- %POW SKIP*
    LEN             <- %LEN SKIP*
    BAND            <- %BAND SKIP*
    BXOR            <- %BXOR SKIP*
    BOR             <- %BOR SKIP*
    SHL             <- %SHL SKIP*
    SHR             <- %SHR SKIP*
    CONCAT          <- %CONCAT SKIP*
    EQ              <- %EQ SKIP*
    LT              <- %LT SKIP*
    GT              <- %GT SKIP*
    NE              <- %NE SKIP*
    LE              <- %LE SKIP*
    GE              <- %GE SKIP*
    ASSIGN          <- %ASSIGN SKIP*
    LPAREN          <- %LPAREN SKIP*
    RPAREN          <- %RPAREN SKIP*
    LBRACKET        <- %LBRACKET SKIP*
    RBRACKET        <- %RBRACKET SKIP*
    LCURLY          <- %LCURLY SKIP*
    RCURLY          <- %RCURLY SKIP*
    SEMICOLON       <- %SEMICOLON SKIP*
    COMMA           <- %COMMA SKIP*
    DOT             <- %DOT SKIP*
    DOTS            <- %DOTS SKIP*
    DBLCOLON        <- %DBLCOLON SKIP*
    COLON           <- %COLON SKIP*
    RARROW          <- %RARROW SKIP*

    NUMBER          <- %NUMBER SKIP*
    STRINGLIT       <- %STRINGLIT SKIP*
    NAME            <- %NAME SKIP*

    -- Synonyms

    NEG             <- SUB
    BNEG            <- BXOR


    NAMEREC         <- [a-zA-Z_][a-zA-Z0-9_]*

    eatTk           <- NAMEREC  /  AND  /  BREAK  /  DO  /  ELSEIF  /  END  /  FALSE  /  FOR  /  FUNCTION  /  GOTO  /  IF  /  IN  /  LOCAL  /  NIL  /  NOT  /  OR  /  RECORD  /  REPEAT  /  RETURN  /  THEN  /  TRUE  /  UNTIL  /  WHILE  /  IMPORT  /  AS  /  FOREIGN  /  BOOLEAN  /  INTEGER  /  FLOAT  /  STRING  /  VALUE  /  .

    --Err_003:
    NameMethod        <- ({} '' -> 'NameMethod') -> adderror  NameMethodRec  ('' -> defaultMethName)
    NameMethodRec     <- (!'(' eatTk)*

    --Err_004: not in parser_spec
    LParPList       <- ({} '' -> 'LParPList') -> adderror  LParPListRec
    LParPListRec    <- (!(NAME  /  ')') eatTk)*
    
    --Err_005: not in parser_spec
    RParPList       <- ({} '' -> 'RParPList') -> adderror  RParPListRec
    RParPListRec    <- (!('while'  /  'return'  /  'repeat'  /  'local'  /  'if'  /  'for'  /  'end'  /  'do'  /  NAME  /  ';'  /  ':'  /  '(') eatTk)*
   
    --Err_006:
    EndFunc         <- ({} '' -> 'EndFunc') -> adderror  EndFuncRec
    EndFuncRec      <- (!('record'  /  'local'  /  'function'  /  NAME  /  !.) eatTk)*

    --Err_007: use NameMethodRec (Err_003)
    NameFunc        <- ({} '' -> 'NameFunc') -> adderror  NameMethodRec  ('' -> defaultFuncName)

    --Err_008, Err_009, and Err_010 refer, respectively, to labels
    --LParPList, RParPlist, and EndFunc, which were also used by rule 'method'
    --and have the same recovery set

    --Err_011:
    AssignVar       <- ({} '' -> 'AssignVar') -> adderror  AssignVarRec
    AssignVarRec    <- (!('~'  /  '{'  /  'true'  /  'not'  /  'nil'  /  'false'  /  NAME  /  NUMBER  /  '-'  /  '('  /  '#'  /  STRINGLIT)  eatTk)*
 
    --Err_012: use EndFuncRec (Err_006)
    ExpVarDec       <- ({} '' -> 'ExpVarDec') -> adderror  EndFuncRec  (P '' -> defaultInt2)  -> number_exp

    --Err_013: Problem: the recovery pattern will not work well, because we reach this label when 'NAME' fails to match (error)
    NameRecord     <- ({} '' -> 'NameRecord') -> adderror  NameRecordRec  ('' -> defaultRecName)
    NameRecordRec  <- (!('end' / NAME) eatTk)*

    --Err_014: use EndFuncRec (Err_006)
    EndRecord       <- ({} '' -> 'EndRecord') -> adderror  EndFuncRec

    --Err_015:
    NameImport      <- ({} '' -> 'NameImport') -> adderror  NameImportRec  ('' -> defaultImportName)
    NameImportRec   <- (!'=' eatTk)*

 
    --Err_016: not in parser_spec
    AssignImport     <- ({} '' -> 'AssignImport') -> adderror  AssignImportRec
    AssignImportRec  <- (!'import' eatTk)*
 
    --Err_017: not in parser_spec
    ImportImport     <- ({} '' -> 'ImportImport') -> adderror  ImportImportRec
    ImportImportRec  <- (!('(' / STRINGLIT) eatTk)* 

    --Err_018:
    StringLParImport     <- ({} '' -> 'StringLParImport') -> adderror  StringLParImportRec  ('' -> defaultStringImportName)
    StringLParImportRec  <- (!')' eatTk)* 

    --Err_019: use EndFuncRec (Err_006)
    RParImport      <- ({} '' -> 'RParImport') -> adderror  EndFuncRec

    --Err_020: use EndFuncRec (Err_006)
    StringImport     <- ({} '' -> 'StringImport') -> adderror  EndFuncRec  ('' -> defaultStringImportName)

    --Err_021, Err_022, Err_023, Err_024, Err_025, Err_026: not in parser_spec
    --There are no specific labels for 'foreign'. The same labels from 'import' are used
 
    --Err_027: TypeFunc
    TypeFunc        <- ({} '' -> 'TypeFunc') -> adderror  TypeFuncRec  (P '') -> TypeInteger
    TypeFuncRec     <- (!('while'  /  'return'  /  'repeat'  /  'local'  /  'if'  /  'for'  /  'end'  /  'do'  /  NAME  /  ';'  /  '(') eatTk)*

    --Err_028: DeclParList -> ParamList
    --The label DeclParList was used by rules decllist and paramlist. I am using distinct labels for each now,
    --because they have different recovery expressions. (It seems ',' should be in both recovery expressions. TODO: check 'first.lua')
    -- use StringLParImportRec (Err_018)
    ParamList       <- ({} '' -> 'ParamList') -> adderror StringLParImportRec 

     --Err_029:
    ParamSemicolon    <- ({} '' -> 'ParamSemicolon') -> adderror  ParamSemicolonRec
    ParamSemicolonRec <- (!('{'  /  'value'  /  'string'  /  'nil'  /  'integer'  /  'float'  /  'boolean'  /  NAME  /  '(') eatTk)*

    --Err_030: TypeDecl -> TypeParam
    --Same reasoning of DeclParList
    TypeParam       <- ({} '' -> 'TypeParam') -> adderror  TypeParamRec  (P '') -> TypeInteger
    TypeParamRec    <- (!(','  /  ')') eatTk)*

    --Err_031: not in parser_spec
    TypeDecl        <- ({} '' -> 'TypeDecl') -> adderror  TypeDeclRec  (P '') -> TypeInteger
    TypeDeclRec     <-  (!('='  /  ',') eatTk)*

    --Err_032: use NameImportRec (Err_015)
    DeclParList     <- ({} '' -> 'DeclParList') -> adderror  NameImportRec

    --Err_XXX: the algorithm did not insert the label corresponding to QualName in rule 'simpletype'
    -- use RCurlyTypeRec (Err_034)
    QualName          <- ({} '' -> 'QualName') -> adderror RCurlyTypeRec  ('' -> defaultQualName)

    --Err_XXX: The algorithm did not insert the first labels TypeType and RCurlyType in the penultimate
    --alternative of 'simpletype', just in the last one. I am using an union of what be the recovery set
    --of all labels TypeType
    --Err_033:
    TypeType        <- ({} '' -> 'TypeType') -> adderror  TypeTypeRec (P '') -> TypeInteger
    TypeTypeRec     <- (!('}' / '.') eatTk)*

    --Err_XXX: See above. Both labels RCurlyType have the same recovery set
    --Err_034:
    RCurlyType      <- ({} '' -> 'RCurlyType') -> adderror  RCurlyTypeRec
    RCurlyTypeRec   <- (!('~='  /  '~'  /  '}'  /  '|'  /  'while'  /  'until'  /  'then'  /  'return'  /  'repeat'  /  'record'  /  'or'  /  'local'  /  'if'  /  'function'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  'and'  /  '^'  /  ']'  /  NAME  /  '>>'  /  '>='  /  '>'  /  '=='  /  '='  /  '<='  /  '<<'  /  '<'  /  ';'  /  ':'  /  '//'  /  '/'  /  '..'  /  '->'  /  '-'  /  ','  /  '+'  /  '*'  /  ')'  /  '('  /  '&'  /  '%%'  /  !.) eatTk)*

    --Err_035: use StringLParImportRec (Err_018)
    TypelistType    <- ({} '' -> 'TypelistType') -> adderror StringLParImportRec

    --Err_036: use RCurlyTypeRec (Err_034)
    RParenTypelist    <- ({} '' -> 'RParenTypelist') -> adderror  RCurlyTypeRec

    --Err_XXX: the algorithm did not insert the two labels corresponding to TypeReturnTypes in rule 'rettype'
    --Label TypeReturnTypes is also used in rule 'type' and its first occurrence in this rule corresponds to Err_038
    --We use recovery expression of Err_038 for all occurrences of TypeReturnTypes

    --Err_037: The original grammar does not have this label
    -- Use ParamSemicolonRec (Err_029)
    Err_037         <- ({} '' -> 'Err_037') -> adderror  ParamSemicolonRec

    --Err_038: The original grammar used TypeReturnTypes here, but the recovery set is different so I introduced label TypeReturnTypes
    --TODO: see why the recovery sets were different
    -- Original recovery expression failed: For a test case, the use of '=' in the recovery expression causes an error, because
    --the input left is not matched as an statment and the repetition block* finishes
    TypeReturnTypes      <- ({} '' -> 'TypeReturnTypes') -> adderror  TypeReturnTypesRec  (P '') -> TypeInteger
    --TypeReturnTypesRec <-	(!('~='  /  '~'  /  '}'  /  '|'  /  'while'  /  'until'  /  'then'  /  'return'  /  'repeat'  /  'record'  /  'or'  /  'local'  /  'if'  /  'function'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  'and'  /  '^'  /  ']'  /  NAME  /  '>>'  /  '>='  /  '>'  /  '=='  /  '='  /  '<='  /  '<<'  /  '<'  /  ';'  /  ':'  /  '//'  /  '/'  /  '..'  /  '-'  /  ','  /  '+'  /  '*'  /  ')'  /  '('  /  '&'  /  '%%'  /  !.) eatTk)*
    TypeReturnTypesRec   <-	(!('~='  /  '~'  /  '}'  /  '|'  /  'while'  /  'until'  /  'then'  /  'return'  /  'repeat'  /  'record'  /  'or'  /  'local'  /  'if'  /  'function'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  'and'  /  '^'  /  ']'  /  NAME  /  '>>'  /  '>='  /  '>'  /  '=='  /  '<='  /  '<<'  /  '<'  /  ';'  /  ':'  /  '//'  /  '/'  /  '..'  /  '-'  /  ','  /  '+'  /  '*'  /  ')'  /  '('  /  '&'  /  '%%'  /  !.) eatTk)*

    --Err_039: Use ParamSemicolonRec (Err_029)
    ColonRecordField    <- ({} '' -> 'ColonRecordField') -> adderror  ParamSemicolonRec 
    
    --Err_040:
    TypeRecordField    <- ({} '' -> 'TypeRecordField') -> adderror  TypeRecordFieldRec  (P '') -> TypeInteger
    TypeRecordFieldRec <- (!('end'  /  NAME  /  ';') eatTk)*
 
    --Err_041:
    EndBlock        <- ({} '' -> 'EndBlock') -> adderror  EndBlockRec
    EndBlockRec     <- (!('while'  /  'until'  /  'return'  /  'repeat'  /  'local'  /  'if'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  NAME  /  ';'  /  '(') eatTk)*

    --Err_042:
    ExpWhile        <- ({} '' -> 'ExpWhile') -> adderror  ExpWhileRec  (P '' -> defaultInt2)  -> number_exp
    ExpWhileRec     <- (!'do' eatTk)*

    --Err_043: use TypeFuncRec (Err_027)
    DoWhile         <- ({} '' -> 'DoWhile') -> adderror  TypeFuncRec

    --Err_044: use EndBlockRec (Err_041)
    EndWhile        <- ({} '' -> 'EndWhile') -> adderror  EndBlockRec

    --Err_045: use AssignVarRec (Err_011)
    UntilRepeat     <- ({} '' -> 'UntilRepeat') -> adderror  AssignVarRec

    --Err_046: use EndBlockRec (Err_041)
    ExpRepeat       <- ({} '' -> 'ExpRepeat') -> adderror  EndBlockRec  (P '' -> defaultInt2)  -> number_exp

    --Err_047:
    ExpIf           <- ({} '' -> 'ExpIf') -> adderror  ExpIfRec  (P '' -> defaultInt2)  -> number_exp
    ExpIfRec        <- (!'then' eatTk)*

    --Err_048:
    ThenIf          <- ({} '' -> 'ThenIf') -> adderror  ThenIfRec
    ThenIfRec       <- (!('while'  /  'return'  /  'repeat'  /  'local'  /  'if'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  NAME  /  ';'  /  '(') eatTk)*

    --Err_049: use EndBlockRec (Err_041)
    EndIf           <- ({} '' -> 'EndIf') -> adderror  EndBlockRec

    --Err_050: use NameImportRec (Err_015)
    DeclFor         <- ({} '' -> 'DeclFor') -> adderror  NameImportRec  (P '' -> defaultDeclName (P '') -> TypeInteger) -> Decl

    --Err_051: use AssignVarRec (Err_011)
    AssignFor       <- ({} '' -> 'AssignFor') -> adderror  AssignVarRec

    --Err_052:
    Exp1For         <- ({} '' -> 'Exp1For') -> adderror  Exp1ForRec  (P '' -> defaultInt2)  -> number_exp
    Exp1ForRec      <- (!',' eatTk)*

    --Err_053: use AssignVarRec (Err_011)
    CommaFor        <- ({} '' -> 'CommaFor') -> adderror  AssignVarRec

    --Err_054:
    Exp2For         <- ({} '' -> 'Exp2For') -> adderror  Exp2ForRec  (P '' -> defaultInt2)  -> number_exp
    Exp2ForRec      <- (!('do'  /  ',') eatTk)*

    --Err_055: use ExpWhileRec (Err_042)
    Exp3For         <- ({} '' -> 'Exp3For') -> adderror  ExpWhileRec  (P '' -> defaultInt2)  -> number_exp

    --Err_056: use TypeFuncRec (Err_027)
    DoFor           <- ({} '' -> 'DoFor') -> adderror  TypeFuncRec

    --Err_057: uses EndBlockRec (Err_041)
    EndFor          <- ({} '' -> 'EndFor') -> adderror  EndBlockRec

    --Err_058: use NameImportRec (Err_015)
    DeclLocal       <- ({} '' -> 'DeclLocal') -> adderror  NameImportRec  {| (P ('' -> defaultDeclName) (P '') -> TypeInteger) -> Decl |}

    --Err_059: use AssignVarRec (Err_011)
    AssignLocal     <- ({} '' -> 'AssignLocal') -> adderror  AssignVarRec

    --Err_060: uses EndBlockRec (Err_041)
    ExpLocal        <- ({} '' -> 'ExpLocal') -> adderror  EndBlockRec  (P '' -> defaultInt2)  -> number_exp

    --Err_XXX: the algorithm did not insert the label corresponding to AssignAssign in rule 'statement'
    AssignAssign    <- ({} '' -> 'AssignAssign') -> adderror  AssignAssignRec
    AssignAssignRec <- (!('~'  /  '{'  /  'true'  /  'not'  /  'nil'  /  'false'  /  NAME  /  NUMBER  /  '-'  /  '('  /  '#'  /  STRINGLIT) eatTk)*

    --Err_XXX: the algorithm did not insert the two labels corresponding to ExpAssign in rule 'statement'
    -- use EndBlockRec (Err_041)
    ExpAssign       <- ({} '' -> 'ExpAssign') -> adderror EndBlockRec  {| (P '' -> defaultInt2)  -> number_exp |}

    --Err_XXX: I renamed the label ExpAssign used in the predicate to ExpAssignPred, since its recovery rule is different
    -- use EndBlockRec (Err_041)
    ExpAssignPred       <- ({} '' -> 'ExpAssignPred') -> adderror (exp ASSIGN) EndBlockRec

    --Err_XXX: the algorithm did not insert the label corresponding to ExpStat in rule 'statement'
    -- uses EndBlockRec
    ExpStat         <- ({} '' -> 'ExpStat') -> adderror (exp) EndBlockRec
    --ExpStatRec      <- (!('while'  /  'until'  /  'return'  /  'repeat'  /  'local'  /  'if'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  NAME  /  ';'  /  '(') eatTk)*

    --Err_061: use ExpIfRec (Err_047)
    ExpElseIf       <- ({} '' -> 'ExpElseIf') -> adderror  ExpIfRec  (P '' -> defaultInt2)  -> number_exp

    --Err_062: use ThenIfRec (Err_048)
    ThenElseIf      <- ({} '' -> 'ThenElseIf') -> adderror  ThenIfRec

    -- Titan grammar use a unique label (OpExp) to expressions e2 (Err_063), e3 (Err_064), e4 (Err_065), e5 (Err_066),
    -- e6 (Err_067), e7 (Err_068), e8 (Err_069, Err_070)), e10 (Err_071), e11 (Err_072, Err_073)
    -- Use the recovery set of e11 in rule e12 (Err_073)
 
    --Err_73: OpExp (e11 in rule e12)
    OpExp     <- ({} '' -> 'OpExp') -> adderror  OpExpRec  (P '' -> defaultInt2)  -> number_exp
    OpExpRec  <- (!('~='  /  '~'  /  '}'  /  '|'  /  'while'  /  'until'  /  'then'  /  'return'  /  'repeat'  /  'record'  /  'or'  /  'local'  /  'if'  /  'function'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  'and'  /  ']'  /  NAME  /  '>>'  /  '>='  /  '>'  /  '=='  /  '<='  /  '<<'  /  '<'  /  ';'  /  '//'  /  '/'  /  '..'  /  '-'  /  ','  /  '+'  /  '*'  /  ')'  /  '('  /  '&'  /  '%%'  /  !.) eatTk)*

    --Err_074:
    NameColonExpSuf    <- ({} '' -> 'NameColonExpSuf') -> adderror  NameColonExpSufRec  ('' -> defaultColonName)
    NameColonExpSufRec <- (!('{'  /  '('  /  STRINGLIT) eatTk)*

    --Err_075:
    FuncArgsExpSuf    <- ({} '' -> 'FuncArgsExpSuf') -> adderror  FuncArgsExpSufRec  {| (P '' -> defaultInt2)  -> number_exp |}
    FuncArgsExpSufRec <- (!('~='  /  '~'  /  '}'  /  '|'  /  '{'  /  'while'  /  'until'  /  'then'  /  'return'  /  'repeat'  /  'record'  /  'or'  /  'local'  /  'if'  /  'function'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  'as'  /  'and'  /  '^'  /  ']'  /  '['  /  NAME  /  '>>'  /  '>='  /  '>'  /  '=='  /  '='  /  '<='  /  '<<'  /  '<'  /  ';'  /  ':'  /  '//'  /  '/'  /  '..'  /  '.'  /  '-'  /  ','  /  '+'  /  '*'  /  ')'  /  '('  /  '&'  /  '%%'  /  !.  /  STRINGLIT) eatTk)*

    --Err_076 and Err_085:
    ExpExpSuf       <- ({} '' -> 'ExpExpSuf') -> adderror  ExpExpSufRec  (P '' -> defaultInt2)  -> number_exp
    ExpExpSufRec    <- (!']' eatTk)*

    --Err_077: use FuncArgsExpSufRec (Err_075)
    RBracketExpSuf    <- ({} '' -> 'RBracketExpSuf') -> adderror  FuncArgsExpSufRec

    --Err_078: use FuncArgsExpSufRec (Err_075)
    NameDotExpSuf    <- ({} '' -> 'NameDotExpSuf') -> adderror  FuncArgsExpSufRec  ('' -> defaultDotName)

    --Err_079: use StringLParImportRec (Err_018)
    ExpSimpleExp     <- ({} '' -> 'ExpSimpleExp') -> adderror  StringLParImportRec  (P '' -> defaultInt2)  -> number_exp

    --Err_080:
    RParSimpleExp    <- ({} '' -> 'RParSimpleExp') -> adderror  RParSimpleExpRec
    RParSimpleExpRec <- (!('~='  /  '~'  /  '}'  /  '|'  /  '{'  /  'while'  /  'until'  /  'then'  /  'return'  /  'repeat'  /  'record'  /  'or'  /  'local'  /  'if'  /  'function'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  'as'  /  'and'  /  '^'  /  ']'  /  '['  /  NAME  /  '>>'  /  '>='  /  '>'  /  '=='  /  '<='  /  '<<'  /  '<'  /  ';'  /  ':'  /  '//'  /  '/'  /  '..'  /  '.'  /  '-'  /  ','  /  '+'  /  '*'  /  ')'  /  '('  /  '&'  /  '%%'  /  !.  /  STRINGLIT) eatTk)*

   --Err_XXX: the algorithm did not insert the label corresponding to CastMissingType in rule 'castexp' (using the FOLLOW of castexp in recovery rule)
   CastMissingType    <- ({} '' -> 'CastMissingType') -> adderror  CastMissingTypeRec  (P '' -> defaultInt2  (P '') -> TypeInteger) -> ExpCast
   CastMissingTypeRec <- (!('~='  /  '~'  /  '}'  /  '|'  /  'while'  /  'until'  /  'then'  /  'return'  /  'repeat'  /  'record'  /  'or'  /  'local'  /  'if'  /  'function'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  'and'  /  '^'  /  ']'  /  NAME  /  '>>'  /  '>='  /  '>'  /  '=='  /  '<='  /  '<<'  /  '<'  /  ';'  /  '//'  /  '/'  /  '..'  /  '-'  /  ','  /  '+'  /  '*'  /  ')'  /  '('  /  '&'  /  '%%'  /  !.) eatTk)*

   --Err_081: use NameImportRec (Err_015)
   ExpVarList       <- ({} '' -> 'ExpVarList') -> adderror  NameImportRec

   --Err_082: use FuncArgsExpSufRec (Err_075)
   RParFuncArgs     <- ({} '' -> 'RParFuncArgs') -> adderror  FuncArgsExpSufRec

   --Err_083:
   ExpExpList      <- ({} '' -> 'ExpExpList') -> adderror  ExpExpListRec  (P '' -> defaultInt2)  -> number_exp
   ExpExpListRec   <- (!('while'  /  'until'  /  'return'  /  'repeat'  /  'local'  /  'if'  /  'for'  /  'end'  /  'elseif'  /  'else'  /  'do'  /  NAME  /  ';'  /  ')'  /  '(') eatTk)*

   --Err_084: use FuncArgsExpSufRec (Err_075)
   RCurlyInitList <- ({} '' -> 'RCurlyInitList') -> adderror  FuncArgsExpSufRec

   --Err_XXX: the algorithm did not insert the label corresponding to ExpFieldList in rule 'fieldlist'
   --use FuncArgsExpSufRec (Err_075)
   ExpFieldList <- ({} '' -> 'ExpFieldList') -> adderror FuncArgsExpSufRec  (P '' -> defaultInt2)  -> number_exp

    --Err_085: the grammar uses ExpExpSuf (Err_076) again. Both uses of this label have the same recovery set

    --Err_086: the grammar used to use RBracketExpSuff, but this instance has very difference recovery set
    --Created label RBracketExpKey: not in parser spec. Use NameImportRec (Err_015)
    RBracketExpKey  <-  ({} '' -> 'RBracketExpKey') -> adderror  NameImportRec

]], defs)


local totalErr = 0
local nSynErr = 0

function parser.parse(filename, input)
    -- Abort if someone calls this non-reentrant parser recursively
    assert(type(filename) == "string")
    assert(THIS_FILENAME == nil)

    THIS_FILENAME = filename
    synerr = {}
    local ast, err, errpos = grammar:match(input)
    --print("ast, err, errpos, synerr", ast, err, errpos, #synerr)
    THIS_FILENAME = nil

    if ast and #synerr == 0 then
        return ast
    else
        assert(ast, "Ouch! We did not get an ast for " .. tostring(err) ..'\n' .. input)
        totalErr = totalErr + #synerr
        nSynErr = nSynErr + 1
        --print("err = ", synerr[1].lab, " #synerr = ", #synerr, " total = ", totalErr, " nErr = ", nSynErr)
        --if #synerr > 1 then
				--	for i, v in ipairs(synerr) do
        --    print(i, v.lab, syntax_errors.errors[v.lab])
        --  end
				--end
        local loc
        if ast then
            --print(parser.pretty_print_ast(ast))
            loc = synerr[1].pos
            err = synerr[1].lab
            --print("loc = ", loc, "err = ", err)
            loc = location.from_pos(filename, input, loc)
        else            
            loc = location.from_pos(filename, input, errpos)
        end
        return false, { label = err, loc = loc }
    end
end

function parser.error_to_string(err)
    local errmsg = syntax_errors.errors[err.label]
    return location.format_error(err.loc, "syntax error: %s", errmsg)
end

function parser.pretty_print_ast(ast)
    return inspect(ast, {
        process = function(item, path)
            if path[#path] ~= inspect.METATABLE then
                return item
            end
        end
    })
end

return parser
