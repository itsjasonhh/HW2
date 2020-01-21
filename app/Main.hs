--Code taken from https://wiki.haskell.org/Parsing_a_simple_imperative_language
module ParseWhile where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
data BExpr = BoolConst Bool
            | Not BExpr
            | And BExpr BExpr
            | Or BExpr BExpr
            | Greater AExpr AExpr
            | Less AExpr AExpr
            | Equal AExpr AExpr
             deriving (Show)
type Var = String
data AExpr = Var String
            | IntConst Integer
            | Neg AExpr
            | Add AExpr AExpr
            | Subtract AExpr AExpr
            | Multiply AExpr AExpr
            deriving (Show)


data Stmt = Seq [Stmt]
           | Assign String AExpr
           | If BExpr Stmt Stmt
           | While BExpr Stmt
           | Skip
           deriving (Show)



languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "not"
                                      , "and"
                                      , "or"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", ":=", "="
                                      , "<", ">", "∧", "∨", "¬"
                                      ]
            }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                     --   parens p
                                     -- takes care of the parenthesis and
                                     -- uses p to parse what's inside them
braces     = Token.braces     lexer
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement
statement :: Parser Stmt
statement =   parens statement
           <|> braces statement
           <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
            <|> whileStmt
            <|> skipStmt
            <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm
bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (Multiply)) AssocLeft
               ]
             , [Infix  (reservedOp "+"   >> return (Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "¬" >> return (Not             ))          ]
             , [Infix  (reservedOp "∧" >> return (And     )) AssocLeft,
                Infix  (reservedOp "∨"  >> return (Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
         <|> (reservedOp "=" >> return Equal)

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e -> error $ show e
    Right r -> r

type Val = Integer
type Store = [(Var,Val)]
evalA :: AExpr -> Store -> Integer
evalA(IntConst n) s = n
evalA(Var x) s = case lookup x s of
                Just v -> v
evalA(Neg x) s = -(evalA x s)
evalA(Add a1 a2) s = evalA a1 s + evalA a2 s
evalA(Subtract a1 a2) s = evalA a1 s - evalA a2 s
evalA(Multiply a1 a2) s = evalA a1 s * evalA a2 s

evalB :: BExpr -> Store -> Bool
evalB(BoolConst b) s = b
evalB(Not b) s = not (evalB b s)
evalB(And b1 b2) s = (evalB b1 s) && (evalB b2 s)
evalB(Or b1 b2) s = (evalB b1 s) || (evalB b2 s)
evalB(Greater a1 a2) s = evalA a1 s > evalA a2 s
evalB(Less a1 a2) s = evalA a1 s < evalA a2 s
evalB(Equal a1 a2) s = evalA a1 s == evalA a2 s

evalStmt :: Stmt -> Store -> Store
evalStmt(Skip) s = s
evalStmt(Assign x a) s = (x,evalA a s):s
evalStmt(While b s) r





main = do
  input <- getLine
  print (parseString input)


