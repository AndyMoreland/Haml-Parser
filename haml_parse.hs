module Main where 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm
import qualified Text.ParserCombinators.Parsec.Token as T

import Data.Monoid
import Control.Applicative hiding (many, (<|>))

type StatefulCharParser a = CharParser Int a

type TagContent = String

data TagIdentifier = Class String | Id String | TagName String deriving (Show)

data Tag = MultiLineTag [TagIdentifier] | SingleLineTag [TagIdentifier] TagContent deriving (Show)

data Tree = BranchingNode String [Tree] 
          | Node String 
          deriving (Show)
                   
lexer = T.makeTokenParser haskellDef
ident = T.identifier    lexer

identifier = T.identifier lexer
dot = T.dot lexer
whiteSpace = T.whiteSpace lexer
lexeme = T.lexeme lexer
symbol = T.symbol lexer
natural = T.natural lexer
parens = T.parens lexer
semi = T.semi lexer
reservedOp = T.reservedOp lexer
whitespace = T.whiteSpace lexer
eol = lexeme (string "\n")

word :: CharParser Int String
word = do 
  many1 letter

manyTill1 p e =  do ms <- manyTill p e
                    case (null ms) of
                      True  -> pzero
                      False -> return ms
                      
parseFile p file =
  do
    content <- readFile file
    run p content
                      
run p input
  = case runParser p 0 "" input of
      Right n  -> putStrLn (concat (map (spitTree "") n))
      Left err -> do{ putStr "parse error at "
; print err }


total :: StatefulCharParser [Tree]
total = do many1 consumeTopLevelChunk
           
consumeTopLevelChunk = do { setState 0 
                          ; topLevelChunk }

topLevelChunk :: StatefulCharParser Tree
topLevelChunk = try (do { spaces <- consumeSpaces
                        ; line <- (manyTill anyChar (string "\n"))
                        ; branches <- many1 doSubChunk
                        ; return (BranchingNode line branches)
                        } ) 
                <|> 
                do {consumeSpaces
                     ; line <- (manyTill anyChar (string "\n"))
                     ; return (Node line)}
                
doSubChunk :: StatefulCharParser Tree
doSubChunk = do 
  spaces <- getState
  -- Check the next indentation level down
  setState $ spaces + 2
  chunk <- topLevelChunk
  -- Reset back to the old indentation level
  setState $ spaces
  return chunk
                  
consumeSpaces = try (do { spaces <- getState
                        ; count spaces (string " ") 
                        ; return spaces })


spitTree :: String -> Tree -> String
spitTree prefix (BranchingNode content branches) = prefix ++ content ++ "\n" ++ (concat (map (spitTree (prefix ++ "  ")) branches))
spitTree prefix (Node content) = prefix ++ content ++ "\n"

    

-- line :: CharParser Int String
-- line = do{ indentCount <- (countParser (string " "))
--          ; updateState (+1)
--          ; many letter
--          }



-- myIdentifier = identifier lexer

-- simple :: Parser Char
-- simple = letter

-- openClose :: Parser Char
-- openClose = do { char '('
--                ; char ')'
--                }
            
-- parens :: Parser ()
-- parens = do 
--   char '('
--   parens
--   char ')' 
--   parens  
--   <|> 
--     return ()
    
-- nesting :: Parser Int
-- nesting = do
--   char '('
--   n <- nesting
--   char ')'
--   m <- nesting
--   return (max (n+1) m)
--   <|> 
--     return 0
         
-- word :: Parser String
-- word = many1 (letter <?> "") <?> "word"

-- separator :: Parser ()
-- separator = skipMany1 (space <|> char ',' <?> "")

-- sentence :: Parser [String]
-- sentence = do{ words <- sepBy1 word separator
--              ; oneOf ".?!" <?> "end of sentence"
--              ; return words
--              }
           
-- -- sentences :: Parser [[String]]
-- -- sentences = do{ sentences <- sepBy1 sentence separator
-- --               ; return sentences
-- --               }

-- expr :: Parser Integer
-- expr = buildExpressionParser table factor <?> "expression"

-- table = [[op "*" (*) AssocLeft, op "/" div AssocLeft],
--          [op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
--           where op s f assoc = Infix (do{ reservedOp s; return f}) assoc
                
-- factor = parens expr
--          <|> natural
--          <?> "simple expression"
         
-- number :: Parser Integer
-- number = do{ ds <- many1 digit
--            ; return (read ds)
--            }
--          <?> "number"
         

-- klasses :: Parser [String]
-- klasses = do { many1 klass } <?> "classes"

-- klass :: Parser String
-- klass = do { dot
--            ; identifier
--            } 
--         <?> "class"

-- -- blocks consisting of blocks or lines 
-- -- lines consist of tag names, and/or class names and/or ids
