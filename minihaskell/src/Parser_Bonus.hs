module Parser_Bonus (
    parseProgram,
    programParser,
) where

import Types 

-- The tokes used in the parser/lexer.
data Token = 
    PPlus | PMinus | PMult | PDiv |
    PAnd | POr | PNot |
    PLtEq | PLt | PGtEq | PGt | PEq | PNeq |
    PIf | PThen | PElse | 
    PAssign | POpenParen | PCloseParen | PResult | PComma | PSemicolon |
    PIdentifier String | PInt Int | PBool Bool
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

lexer :: String -> [Token]
lexer [] = []
lexer ('<':'=':cs) = PLtEq : lexer cs
lexer ('<':cs) = PLt : lexer cs
lexer ('>':'=':cs) = PGtEq : lexer cs
lexer ('>':cs) = PGt : lexer cs
lexer ('=': '=': cs) = PEq : lexer cs
lexer ('/': '=': cs) = PNeq : lexer cs
lexer ('+':cs) = PPlus : lexer cs
lexer ('-':cs) = PMinus : lexer cs
lexer ('*':cs) = PMult : lexer cs
lexer ('/':cs) = PDiv : lexer cs
lexer ('&':'&':cs) = PAnd : lexer cs
lexer ('|':'|':cs) = POr : lexer cs
lexer ('=':cs) = PAssign : lexer cs
lexer ('(':cs) = POpenParen : lexer cs
lexer (')':cs) = PCloseParen : lexer cs
lexer (',':cs) = PComma : lexer cs
lexer (';':xs) = PSemicolon : lexer xs
lexer (' ':cs) = lexer cs
lexer ('\t':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer (c:cs)
    | isDigit c = PInt (read (c:takeWhile isDigit cs)) : lexer (dropWhile isDigit cs)
    | isAlpha c = case span isAlpha (c:cs) of
        ("True", rest) -> PBool True : lexer rest
        ("False", rest) -> PBool False : lexer rest
        ("result", rest) -> PResult : lexer rest
        ("if", rest) -> PIf : lexer rest
        ("then", rest) -> PThen : lexer rest
        ("else", rest) -> PElse : lexer rest
        ("not", rest) -> PNot : lexer rest
        (ident, rest) -> PIdentifier ident : lexer rest
    | otherwise = error $ "Invalid character: " ++ [c]

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isChar :: Char -> Bool
isChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

isAlpha :: Char -> Bool
isAlpha c = isChar c || isDigit c

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- A simple main function that reads a file and prints the tokens that are returned by the lexer.
-- main :: IO ()
-- main = do 
--     files <- getFile 
--     let file = head files
--     input <- readFile file
--     putStrLn $ show $ lexer input


-- A simple main function that reads the file and prints the AST that is returned by the function programParser.
-- main :: IO ()
-- main = do
--     files <- getFile
--     let file = head files
--     input <- readFile file
--     print (programParser input)

-- getFile :: IO [String]
-- getFile = return ["../test/test8.txt"]


-- Parse the program and returns an AST.
-- Note: The return value is an Either monad, which is either a String (error message) or an AST but it is not used for error handling as it was not asked in the assignment.
programParser :: String -> Either String FProgram
programParser input = parseProgram $ lexer input


-- Parses the program. Used by programParser, it returns Either a String (error message) or an AST.
parseProgram :: [Token] -> Either String FProgram
parseProgram tokens =
    case tokens of
        [] -> error "Unexpected end of input"
        (PResult:rest) ->
            let (expr, defs, tokens') = parseResult rest
                in case tokens' of
                    [] -> Right (expr, defs)
                    _ -> Left "Error parsing program."
        _ -> Left "Error parsing program."


-- Parses the result expression and the function definitions.
parseResult :: [Token] -> (FExpr, [FDefinition], [Token])
parseResult tokens =
    case tokens of
        [] -> error "Unexpected end of input at result statement."
        (PAssign:rest) ->
            let (expr, tokens') = parseExpression rest
                in case tokens' of
                    (PSemicolon:rest') ->
                        let (defs, tokens'') = parseDefinitions rest'
                            in (expr, defs, tokens'')
                    _ -> error "Missing semicolon from result statement."
        _ -> error "Missing assignment from result statement."


-- Parses a list of function definitions as they are given in the program after the "result" expression.
parseDefinitions :: [Token] -> ([FDefinition], [Token])
parseDefinitions tokens =
    case tokens of
        [] -> ([], tokens)                                  -- No more definitions.
        (PIdentifier _:rest) ->                             -- A function definition.
            let (def, tokens') = parseDefinition tokens
                in let (defs, tokens'') = parseDefinitions tokens'
                    in (def:defs, tokens'')
        _ -> ([], tokens)


-- Parses a function definition. 
-- Returns the function definition and the remaining tokens.
parseDefinition :: [Token] -> (FDefinition, [Token])
parseDefinition tokens =
    case tokens of
        [] -> error "Unexpected end of input"
        (PIdentifier name:rest) ->
            case rest of
                (POpenParen:rest') ->
                    let (params, tokens') = parseParameters rest'
                        in case tokens' of
                            (PAssign:rest'') ->
                                let (expr, tokens'') = parseExpression rest''
                                    in case tokens'' of
                                        (PSemicolon:rest''') -> ((name, params, expr), rest''')
                                        _ -> error "Missing semicolon from function definition."
                            _ -> error "Missing assignment from function definition."
                _ -> error "Missing open parenthesis from function definition."
        _ -> error "Missing identifier from function definition."


-- Parses a list of parameters as they are given in the function's definition.
parseParameters :: [Token] -> ([String], [Token])
parseParameters tokens =
    case tokens of
        (PCloseParen : rest) -> ([], rest)                        -- No parameters
        (PIdentifier param : rest) ->                             -- One or more parameters
            case rest of
                (PComma : rest') -> 
                    let (params, tokens') = parseParameters rest'
                        in (param:params, tokens')
                (PCloseParen : rest') -> ([param], rest')
                _ -> error "Missing comma or close parenthesis from function definition."

    


-- Parses an expression and returns the FExpr and the remaining tokens.
parseExpression :: [Token] -> (FExpr, [Token])
parseExpression tokens =
  let (expr, tokens') = parseFactor tokens
  in parseTerm expr tokens'


-- Parses a factor and returns the FExpr and the remaining tokens.
-- A factor for this specifix grammar can be an Int,a Bool, a variable, a function call, a parenthesized expression, a unary operation, or an if-then-else statement.
parseFactor :: [Token] -> (FExpr, [Token])
parseFactor tokens =
  case tokens of
    [] -> error "Unexpected end of input"
    (PInt n : rest) -> (FNum n, rest)                       -- Int
    (PBool b : rest) -> (FBool b, rest)                     -- Bool
    (PIdentifier s : POpenParen : rest') ->                 -- Function call
      let (args, tokens') = parseArgs rest'
      in case tokens' of
        (PCloseParen : rest'') -> (FCall s args, rest'')
        _ -> (FCall s args, tokens')
    (PIdentifier s : rest) -> (FVar s, rest)                -- Variable
    (POpenParen : rest) ->                                  -- Parenthesized expression
      let (expr, tokens') = parseExpression rest
      in case tokens' of
        (PCloseParen : rest') -> (FParens expr, rest')
        _ -> error "Missing PCloseParen."
    (PMinus : rest) ->                                      -- Unary operations...           
      let (expr, tokens') = parseFactor rest
      in (FUnaryOp Negative expr, tokens')
    (PNot : rest) ->
      let (expr, tokens) = parseFactor rest
      in (FUnaryOp Not expr, tokens)
    (PPlus : rest) ->
      let (expr, tokens) = parseFactor rest
      in (FUnaryOp Positive expr, tokens)
    (PIf : rest) ->                                        -- If-then-else statement
      let (cond, tokens') = parseExpression rest
      in case tokens' of
        (PThen : rest') ->
          let (then_expr, tokens'') = parseExpression rest'
          in case tokens'' of
            (PElse : rest'') ->
              let (else_expr, tokens''') = parseExpression rest''
              in (FIfThenElse cond then_expr else_expr, tokens''')
            _ -> error "Missing else in if-then-else statement."
        _ -> error "Missing then in if-then-else statement."
    _ -> error "Unexpected token in parseFactor."
    

-- Parses the argument string, in result expression, and returns a list of tokens and FExpressions.
parseArgs :: [Token] -> ([FExpr], [Token])
parseArgs tokens =
    case tokens of
        (PCloseParen:rest) -> ([], rest)
        (PComma:_) -> error "Missing expression before comma"
        _ ->
            let (expr, tokens') = parseExpression tokens
            in case tokens' of
                (PCloseParen:rest) -> ([expr], rest)
                (PComma:rest) ->
                    let (exprs, tokens'') = parseArgs rest
                    in (expr:exprs, tokens'')
                _ -> ([expr], tokens')


-- Parses a term and returns the FExpr and the remaining tokens.
parseTerm :: FExpr -> [Token] -> (FExpr, [Token])
parseTerm left_expr tokens =
  case tokens of
    [] -> (left_expr, [])
    (PPlus:rest) ->                        -- Binary operations...
      let (right_expr, tokens'') = parseFactor rest
          (left_expr', tokens''') = (FBinaryOp Plus left_expr right_expr, tokens'')
      in parseTerm left_expr' tokens'''
    (PMinus:rest) ->
      let (right_expr, tokens'') = parseFactor rest
          (left_expr', tokens''') = (FBinaryOp Minus left_expr right_expr, tokens'')
      in parseTerm left_expr' tokens'''
    (PMult : rest) ->
      let (right_expr, tokens'') = parseFactor rest
          (left_expr', tokens''') = (FBinaryOp Mult left_expr right_expr, tokens'')
      in parseTerm left_expr' tokens'''
    (PDiv : rest) ->
      let (right_expr, tokens'') = parseFactor rest
          (left_expr', tokens''') = (FBinaryOp Div left_expr right_expr, tokens'')
      in parseTerm left_expr' tokens'''
    (PLtEq : rest) ->                    -- Comparison operations...
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FCompOp LtEq left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (PLt : rest) ->
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FCompOp Lt left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (PGtEq : rest) ->
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FCompOp GtEq left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (PGt : rest) ->
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FCompOp Gt left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (PEq : rest) ->
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FCompOp Eq left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (PNeq : rest) ->
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FCompOp Neq left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (PAnd : rest) ->                -- Boolean operations...
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FBooleanOp And left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (POr : rest) ->
        let (right_expr, tokens'') = parseFactor rest
            (left_expr', tokens''') = (FBooleanOp Or left_expr right_expr, tokens'')
        in parseTerm left_expr' tokens'''
    (PIf : rest) ->                -- If-then-else statement
        let (cond, tokens') = parseExpression rest
        in case tokens' of
            (PThen : rest') ->
                let (then_expr, tokens'') = parseExpression rest'
                in case tokens'' of
                    (PElse : rest'') ->
                        let (else_expr, tokens''') = parseExpression rest''
                        in (FIfThenElse cond then_expr else_expr, tokens''')
                    _ -> error "Missing else from if-then-else statement."
            _ -> error "Missing then from if-then-else statement."
    _ -> (left_expr, tokens)
