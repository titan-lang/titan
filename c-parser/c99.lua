-- C99 grammar written in lpeg.re.
-- Adapted and translated from plain LPeg grammar for C99
-- written by Wesley Smith https://github.com/Flymir/ceg
--
-- Copyright (c) 2009 Wesley Smith
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

-- Reference used in the original and in this implementation:
-- http://www.open-std.org/JTC1/SC22/wg14/www/docs/n1124.pdf

local c99 = {}

local re = require("relabel")

local defs = {}

local location = require("titan-compiler.location")

c99.tracing = false

defs["trace"] = function(s, i)
    if c99.tracing then
        local line, col = location.get_line_number(s, i)
        print("TRACE", line, col, "[[" ..s:sub(i, i+ 256):gsub("\n.*", "") .. "]]")
    end
    return true
end

local typedefs = {}

local function elem(xs, e)
    for _, x in ipairs(xs) do
        if e == x then
            return true
        end
    end
    return false
end

defs["store_typedef"] = function(s, i, decl)
    if elem(decl.spec, "typedef") then
        if not (decl.ids and decl.ids[1] and decl.ids[1].decl) then
            return false
        end
        local name = decl.ids[1].decl.name or decl.ids[1].decl.declarator.name
        if name then
            typedefs[name] = true
        end
    end
    return true, decl
end

defs["is_typedef"] = function(s, i, id)
    --print("is " .. id .. " a typedef? " .. tostring(not not typedefs[id]))
    return typedefs[id], typedefs[id] and id
end

defs["empty_table"] = function()
    return true, {}
end

--==============================================================================
-- Lexical Rules (used in both preprocessing and language processing)
--==============================================================================

local lexical_rules = [[--lpeg.re

TRACE <- ({} => trace)

EMPTY_TABLE <- ("" => empty_table)

--------------------------------------------------------------------------------
-- Identifiers

IDENTIFIER <- { identifierNondigit (identifierNondigit / [0-9])* } _
identifierNondigit <- [a-zA-Z_]
                    / universalCharacterName

identifierList <- IDENTIFIER ("," _ IDENTIFIER)*

--------------------------------------------------------------------------------
-- Universal Character Names

universalCharacterName <- "\u" hexQuad
                        / "\U" hexQuad hexQuad
hexQuad <- hexDigit^4

--------------------------------------------------------------------------------
-- String Literals

STRING_LITERAL <- { ('"' / 'L"') sChar* '"' } _

sChar <- (!["\%nl] .) / escapeSequence

--------------------------------------------------------------------------------
-- Escape Sequences

escapeSequence <- simpleEscapeSequence
                / octalEscapeSequence
                / hexEscapeSequence
                / universalCharacterName

simpleEscapeSequence <- "\" ['"?\abfnrtv]

octalEscapeSequence <- "\" [0-7] [0-7]^-2

hexEscapeSequence <- "\x" hexDigit+

--------------------------------------------------------------------------------
-- Constants

INTEGER_CONSTANT <- { ( hexConstant integerSuffix?
                    / octalConstant integerSuffix?
                    / decimalConstant integerSuffix?
                    ) } _

decimalConstant <- [1-9] digit*
octalConstant <- "0" [0-7]*
hexConstant <- ("0x" / "0X") hexDigit+

digit <- [0-9]
hexDigit <- [0-9a-fA-F]

integerSuffix <- unsignedSuffix longSuffix?
               / unsignedSuffix longLongSuffix
               / longLongSuffix unsignedSuffix?
               / longSuffix unsignedSuffix?

unsignedSuffix <- [uU]
longSuffix <- [lL]
longLongSuffix <- "ll" / "LL"

FLOATING_CONSTANT <- { ( decimalFloatingConstant
                       / hexFloatingConstant
                       ) } _

decimalFloatingConstant <- fractionalConstant exponentPart? floatingSuffix?
                         / digit+ exponentPart floatingSuffix?

hexFloatingConstant <- ("0x" / "0X" ) ( hexFractionalConstant binaryExponentPart floatingSuffix?
                                      /  hexDigit+ binaryExponentPart floatingSuffix? )

fractionalConstant <- digit* "." digit+
                    / digit "."

exponentPart <- [eE] [-+]? digit+

hexFractionalConstant <- hexDigit+? "." hexDigit+
                       / hexDigit+ "."

binaryExponentPart <- [pP] digit+

floatingSuffix <- [flFL]

CHARACTER_CONSTANT <- { ("'" / "L'") cChar+ "'" } _

cChar <- (!['\%nl] .) / escapeSequence

enumerationConstant <- IDENTIFIER

]]

local common_expression_rules = [[--lpeg.re

--------------------------------------------------------------------------------
-- Common Expression Rules

multiplicativeExpression <- {| castExpression           ({:op: [*/%]                     :} _ castExpression                        )* |}
additiveExpression       <- {| multiplicativeExpression ({:op: [-+]                      :} _ multiplicativeExpression              )* |}
shiftExpression          <- {| additiveExpression       ({:op: ("<<" / ">>")             :} _ additiveExpression                    )* |}
relationalExpression     <- {| shiftExpression          ({:op: (">=" / "<=" / "<" / ">") :} _ shiftExpression                       )* |}
equalityExpression       <- {| relationalExpression     ({:op: ("==" / "!=")             :} _ relationalExpression                  )* |}
bandExpression           <- {| equalityExpression       ({:op: "&"                       :} _ equalityExpression                    )* |}
bxorExpression           <- {| bandExpression           ({:op: "^"                       :} _ bandExpression                        )* |}
borExpression            <- {| bxorExpression           ({:op: "|"                       :} _ bxorExpression                        )* |}
andExpression            <- {| borExpression            ({:op: "&&"                      :} _ borExpression                         )* |}
orExpression             <- {| andExpression            ({:op: "||"                      :} _ andExpression                         )* |}
conditionalExpression    <- {| orExpression             ({:op: "?"                       :} _ expression ":" _ conditionalExpression)? |}

constantExpression <- conditionalExpression

]]

--==============================================================================
-- Language Rules (Phrase Structure Grammar)
--==============================================================================

local language_rules = [[--lpeg.re

--------------------------------------------------------------------------------
-- External Definitions

translationUnit <- %s* {| externalDeclaration+ |} "$EOF$"

externalDeclaration <- functionDefinition
                     / declaration

functionDefinition <- {| {:spec: {| declarationSpecifier+ |} :} {:function: declarator :} {:decls: declaration* :} {:code: compoundStatement :} |}

--------------------------------------------------------------------------------
-- Declarations

declaration <- {| {:spec: {| declarationSpecifier+ |} :} ({:ids: initDeclarationList :})? gccExtensionSpecifier* ";" _ |} => store_typedef

declarationSpecifier <- storageClassSpecifier
                      / typeSpecifier
                      / typeQualifier
                      / functionSpecifier

initDeclarationList <- {| initDeclarator ("," _ initDeclarator)* |}

initDeclarator <- {| {:decl: declarator :} ("=" _ {:value: initializer :} )? |}

gccExtensionSpecifier <- "__attribute__" _ "(" _ "(" _ gccAttributeList ")" _ ")" _
                       / gccAsm

gccAsm <- "__asm__" _ "(" _ (STRING_LITERAL / ":" _ / expression)+ ")" _

gccAttributeList <- gccAttributeItem ("," _ gccAttributeItem )*

gccAttributeItem <- IDENTIFIER ("(" _ (expression ("," _ expression)*)? ")" _)?
                  / ""

storageClassSpecifier <- { "typedef"  } _
                       / { "extern"   } _
                       / { "static"   } _
                       / { "auto"     } _
                       / { "register" } _

typeSpecifier <- typedefName
               / { "void"     } _
               / { "char"     } _
               / { "short"    } _
               / { "int"      } _
               / { "long"     } _
               / { "float"    } _
               / { "double"   } _
               / { "signed"   } _
               / { "unsigned" } _
               / { "_Bool"    } _
               / { "Complex"  } _
               / structOrUnionSpecifier
               / enumSpecifier

typeQualifier <- { "const"    } _
               / { "restrict" } _
               / { "volatile" } _

functionSpecifier <- { "inline" } _

structOrUnionSpecifier <- {| {:type: structOrUnion :} ({:id: IDENTIFIER :})? "{" _ {:fields: {| structDeclaration+ |} :} "}" _ |}
                        / {| {:type: structOrUnion :}  {:id: IDENTIFIER :}                                                     |}

structOrUnion <- { "struct" } _
               / { "union"  } _

anonymousUnion <- {| {:type: {| {:type: { "union" } :} _ "{" _ {:fields: {| structDeclaration+ |} :} "}" _ |} :} |} ";" _

structDeclaration <- anonymousUnion
                   / {| {:type: {| specifierQualifier+ |} :} {:ids: {| structDeclaratorList |} :} |} ";" _

specifierQualifier <- typeSpecifier
                    / typeQualifier

structDeclaratorList <- structDeclarator ("," _ structDeclarator)*

structDeclarator <- declarator (":" _ constantExpression)?

enumSpecifier <- {| {:type: enum :} ({:id: IDENTIFIER :})? "{" _ {:values: {| enumeratorList |} :} ("," _)? "}" _ |}
               / {| {:type: enum :}  {:id: IDENTIFIER :}                                                          |}

enum <- { "enum" } _

enumeratorList <- enumerator ("," _ enumerator)*

enumerator <- {| {:id: enumerationConstant :} ("=" _ {:value: constantExpression :})? |}

declarator <- {| pointer? directDeclarator |}

directDeclarator <- {:name: IDENTIFIER :} ddRec
                  / "(" _ {:declarator: declarator :} ")" _ ddRec
ddRec <- "[" _ {| {:idx: typeQualifier* assignmentExpression?          :} |} "]" _ ddRec
       / "[" _ {| {:idx: { "static" } _ typeQualifier* assignmentExpression :} |} "]" _ ddRec
       / "[" _ {| {:idx: typeQualifier+ { "static" } _ assignmentExpression :} |} "]" _ ddRec
       / "[" _ {| {:idx: typeQualifier* { "*" } _                           :} |} "]" _ ddRec
       / "(" _ {:params: {| parameterTypeList |} :} ")" _ ddRec
       / "(" _ {:params: {| identifierList?   |} :} ")" _ ddRec
       / ""

pointer <- ({ "*" } _ typeQualifier*)+

parameterTypeList <- parameterList ("," _ { "..." } _)?

parameterList <- parameterDeclaration ("," _ parameterDeclaration)*

parameterDeclaration <- {| {:param: {| {:type: {| declarationSpecifier+ |} :} {:id: (declarator / abstractDeclarator?) :} |} :} |}

typeName <- specifierQualifier+ abstractDeclarator?

abstractDeclarator <- pointer
                    / pointer? directAbstractDeclarator

directAbstractDeclarator <- ("(" _ abstractDeclarator ")" _) directAbstractDeclarator2*
                          / directAbstractDeclarator2+
directAbstractDeclarator2 <- "[" _ assignmentExpression? "]" _
                           / "[" _ "*" _ "]" _
                           / "(" _ parameterTypeList? ")" _

typedefName <- IDENTIFIER => is_typedef

initializer <- assignmentExpression
             / "{" _ initializerList ("," _)? "}" _

initializerList <- initializerList2 ("," _ initializerList2)*
initializerList2 <- designation? initializer

designation <- designator+ "=" _

designator <- "[" _ constantExpression "]" _
            / "." _ IDENTIFIER

--------------------------------------------------------------------------------
-- Statements

statement <- labeledStatement
           / compoundStatement
           / expressionStatement
           / selectionStatement
           / iterationStatement
           / jumpStatement
           / gccAsm ";" _

labeledStatement <- IDENTIFIER ":" _ statement
                  / "case" _ constantExpression ":" _ statement
                  / "default" _ ":" _ statement

compoundStatement <- "{" _ blockItem+ "}" _

blockItem <- declaration
           / statement

expressionStatement <- expression? _ ";" _

selectionStatement <- "if" _ "(" _ expression ")" _ statement "else" _ statement
                    / "if" _ "(" _ expression ")" _ statement
                    / "switch" _ "(" _ expression ")" _ statement

iterationStatement <- "while" _ "(" _ expression ")" _ statement
                    / "do" _ statement "while" _ "(" _ expression ")" _ ";" _
                    / "for" _ "(" _ expression? ";" _ expression? ";" _ expression? ")" _ statement
                    / "for" _ "(" _ declaration expression? ";" _ expression? ")" _ statement

jumpStatement <- "goto" _ IDENTIFIER ";" _
               / "continue" _ ";" _
               / "break" _ ";" _
               / "return" _ expression? ";" _

--------------------------------------------------------------------------------
-- Advanced Language Expression Rules
-- (which require type names)

postfixExpression <- {| primaryExpression peRec |}
                   / {| "(" _ {:struct: typeName :} ")" _ "{" _ initializerList ("," _)? "}" _ peRec |}

sizeofOrPostfixExpression <- {| {:op: "sizeof" :} _ "(" _ typeName ")" _ |}
                           / {| {:op: "sizeof" :} _ unaryExpression      |}
                           / postfixExpression

castExpression <- "(" _ typeName ")" _ castExpression
                / unaryExpression

]]

--==============================================================================
-- Language Expression Rules
--==============================================================================

local language_expression_rules = [[--lpeg.re

--------------------------------------------------------------------------------
-- Language Expression Rules
-- (rules which differ from preprocessing stage)

expression <- {| assignmentExpression ("," _ assignmentExpression)* |}

constant <- ( FLOATING_CONSTANT
            / INTEGER_CONSTANT
            / CHARACTER_CONSTANT
            / enumerationConstant
            )

primaryExpression <- IDENTIFIER
                   / constant
                   / STRING_LITERAL+
                   / "(" _ expression ")" _

peRec <- {| "[" _ {:idx: expression :} "]" _ peRec |}
       / {| "(" _ {:args: (argumentExpressionList / EMPTY_TABLE) :} ")" _ peRec |}
       / {| "." _ {:dot: IDENTIFIER :} peRec |}
       / {| "->" _ {:arrow: IDENTIFIER :} peRec |}
       / { "++" } _ peRec
       / { "--" } _ peRec
       / ""

argumentExpressionList <- assignmentExpression ("," _ assignmentExpression)*

unaryExpression <- {| {:preop: prefixOp :} unaryExpression     |}
                 / {| {:unop: unaryOperator :} castExpression  |}
                 / sizeofOrPostfixExpression

prefixOp <- { "++" } _
          / { "--" } _

unaryOperator <- { [-+~!*&] } _

assignmentExpression <- conditionalExpression
                      / unaryExpression assignmentOperator assignmentExpression

assignmentOperator <- "=" _
                    / "*=" _
                    / "/=" _
                    / "%=" _
                    / "+=" _
                    / "-=" _
                    / "<<=" _
                    / ">>=" _
                    / "&=" _
                    / "^=" _
                    / "|=" _

--------------------------------------------------------------------------------
-- Language whitespace

_ <- %s+
S <- %s+

]]

local simplified_language_expression_rules = [[--lpeg.re

--------------------------------------------------------------------------------
-- Simplified Language Expression Rules
-- (versions that do not require knowledge of type names)

postfixExpression <- {| primaryExpression peRec |}

sizeofOrPostfixExpression <- postfixExpression

castExpression <- unaryExpression

]]

--==============================================================================
-- Preprocessing Rules
--==============================================================================

local preprocessing_rules = [[--lpeg.re

preprocessingLine <- _ ( "#" _ {| directive |} _
                       / {| preprocessingTokens? |} _
                       / "#" _ preprocessingTokens? _ -- non-directive, ignore
                       )

preprocessingTokens <- {| (preprocessingToken _)+ |}

directive <- {:directive: "if"      :} S {:exp: preprocessingTokens :}
           / {:directive: "ifdef"   :} S {:id: IDENTIFIER :}
           / {:directive: "ifndef"  :} S {:id: IDENTIFIER :}
           / {:directive: "elif"    :} S {:exp: preprocessingTokens :}
           / {:directive: "else"    :}
           / {:directive: "endif"   :}
           / {:directive: "include" :} S {:exp: headerName :}
           / {:directive: "define"  :} S {:id: IDENTIFIER :} "(" _ {:args: {| defineArgs |} :} _ ")" _ {:repl: replacementList :}
           / {:directive: "define"  :} S {:id: IDENTIFIER :} _ {:repl: replacementList :}
           / {:directive: "undef"   :} S {:id: IDENTIFIER :}
           / {:directive: "line"    :} S {:line: preprocessingTokens :}
           / {:directive: "error"   :} S {:error: preprocessingTokens? :}
           / {:directive: "pragma"  :} S {:pragma: preprocessingTokens? :}
           / gccDirective
           / ""

gccDirective <- {:directive: "include_next" :} S {:exp: headerName :}

defineArgs <- { "..." }
            / identifierList _ "," _ { "..." }
            / identifierList?

replacementList <- {| (preprocessingToken _)* |}

preprocessingToken <- IDENTIFIER
                    / preprocessingNumber
                    / CHARACTER_CONSTANT
                    / STRING_LITERAL
                    / punctuator

headerName <- {| {:mode: "<" -> "system" :} { (![%nl>] .)+ } ">" |}
            / {| {:mode: '"' -> "quote" :} { (![%nl"] .)+ } '"' |}

preprocessingNumber <- { ("."? digit) ( digit
                                      / identifierNondigit
                                      / [eEpP] [-+]
                                      / "."
                                      )* }

punctuator <- { digraphs / '...' / '<<=' / '>>=' /
                '##' / '<<' / '>>' / '->' / '++' / '--' / '&&' / '||' / '<=' / '>=' /
                '==' / '!=' / '*=' / '/=' / '%=' / '+=' / '-=' / '&=' / '^=' / '|=' /
                '#' / '[' / ']' / '(' / ')' / '{' / '}' / '.' / '&' /
                '*' / '+' / '-' / '~' / '!' / '/' / '%' / '<' / '>' /
                '^' / '|' / '?' / ':' / ';' / ',' / '=' }

digraphs <- '%:%:' -> "##"
          / '%:' -> "#"
          / '<:' -> "["
          / ':>' -> "]"
          / '<%' -> "{"
          / '%>' -> "}"

--------------------------------------------------------------------------------
-- Preprocessing whitespace

_ <- %s*
S <- %s+

]]

--==============================================================================
-- Preprocessing Expression Rules
--==============================================================================

local preprocessing_expression_rules = [[--lpeg.re

--------------------------------------------------------------------------------
-- Preprocessing Expression Rules
-- (rules which differ from language processing stage)

expression <- constantExpression

constant <- FLOATING_CONSTANT
          / INTEGER_CONSTANT
          / CHARACTER_CONSTANT

primaryExpression <- IDENTIFIER
                   / constant
                   / {| "(" _ expression _ ")" _ |}

postfixExpression <- primaryExpression peRec
peRec <- "(" _ argumentExpressionList? ")" _ peRec
       / ""

argumentExpressionList <- {| { expression } ("," _ { expression } )* |}

unaryExpression <- {| {:op: unaryOperator :} {:exp: unaryExpression :} |}
                 / primaryExpression

unaryOperator <- { [-+~!] } _
               / { "defined" } _

castExpression <- unaryExpression

--------------------------------------------------------------------------------
-- Preprocessing expression whitespace

_ <- %s*
S <- %s+

]]

local preprocessing_grammar = re.compile(
    preprocessing_rules ..
    lexical_rules, defs)

local preprocessing_expression_grammar = re.compile(
    preprocessing_expression_rules ..
    lexical_rules ..
    common_expression_rules, defs)

local language_expression_grammar = re.compile(
    language_expression_rules ..
    simplified_language_expression_rules ..
    lexical_rules ..
    common_expression_rules, defs)

local language_grammar = re.compile(
    language_rules ..
    language_expression_rules ..
    lexical_rules ..
    common_expression_rules, defs)

local function match(grammar, subject)
    local res, err, pos = grammar:match(subject)
    if res == nil then
        local l, c = re.calcline(subject, pos)
        local fragment = subject:sub(pos, pos+20)
        return res, err, l, c, fragment
    end
    return res
end

function c99.match_language_grammar(subject)
    typedefs = {}
    return match(language_grammar, subject)
end

function c99.match_language_expression_grammar(subject)
    return match(language_expression_grammar, subject)
end

function c99.match_preprocessing_grammar(subject)
    return match(preprocessing_grammar, subject)
end

function c99.match_preprocessing_expression_grammar(subject)
    return match(preprocessing_expression_grammar, subject)
end

return c99
