import Data.Char (isAlphaNum)
import Data.List
import System.Environment (getArgs)
import System.IO
import Text.Parsec
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (decimal, floating3)
import Text.Parsec.Error

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  case parse parseFile "PixllParser" input of
    Left err -> hPutStrLn stderr $ showParseError (head args) input err
    Right val -> mapM_ putStrLn . lines $ compileToPython val

showParseError :: FilePath -> String -> ParseError -> String
showParseError file content err =
  let pos = errorPos err
      lineNum = sourceLine pos
      col = sourceColumn pos
      messages = errorMessages err
      padLen = length . show $ lineNum + 2
      line =
        if length (lines content) > lineNum + 1
        then lines content !! (lineNum - 1)
        else ""
      dashes = take (col - 1) (cycle " ")
  in "Pixll syntax error in " <> file <> " at line " <> show lineNum <> " (column " <> show col <> ")" <> 
      "\n" <> dashes <> "v\n" <>
      line <> 
      "\n" <> dashes <> "^" <>
      showErrorMessages "\n" "Unknown" "\nExpecting one of:\n" "Did not expect" "End of file" messages <>
      "\n\nIn context:\n" <>
      safeLineNum padLen content (lineNum - 3) <> "\n" <>
      safeLineNum padLen content (lineNum - 2) <> "\n" <>
      leftPad padLen (lineNum) <> " X " <> line <> "\n" <>
      safeLineNum padLen content lineNum <> "\n" <>
      safeLineNum padLen content (lineNum + 1)

leftPad :: Int -> Int -> String
leftPad padLen x = reverse . take padLen $ reverse (show x) ++ cycle " "

safeLineNum :: Int -> String -> Int -> String
safeLineNum padLen content num =
  if length (lines content) > num && num >= 0
  then leftPad padLen (num + 1) <> " | " <> lines content !! (num)
  else ""

type LightPattern = String
type ArrayName = String
type ProcedureName = String
type TransformName = String
type TransformationName = String

data Expr =
    Paren Expr
  | Neg Expr
  | Sqrt Expr
  | Rnd Expr
  | Exp Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | IntDiv Expr Expr
  | Mod Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Number Int
  | Red
  | Blue
  | Green
  | Arg
  deriving Show

data Transform = Transform TransformName Int
  deriving Show

data PixllVal
  = Array ArrayName LightPattern ArrayName ArrayName
  | Comment String
  | Procedure ProcedureName ArrayName [Transform] Int Double
  | Transformation TransformationName Expr Expr Expr
  deriving Show

-- Compilation
compileToPython :: [PixllVal] -> String
compileToPython vals = "from raspilights import *\n\n" ++ concatMap toPython vals ++ 
  "if __name__ == '__main__':\n" ++
  "    set_mode('software')\n\n" ++
  "    procedures = [" ++
  (intercalate "," $ map procedureToString $ filter isProcedure vals) ++ "]\n\n" ++
  "    while True:\n" ++
  "        proc = random.choice(procedures)\n" ++
  "        sh = random.choice([show, reversed_show])\n" ++
  "        proc(sh)\n"

isProcedure :: PixllVal -> Bool
isProcedure Procedure{} = True
isProcedure _ = False

procedureToString :: PixllVal -> String
procedureToString (Procedure name _ _ _ _) = name
procedureToString _ = error "procedureToString can only convert procedures"

py :: Expr -> String
py (Paren x) = "(" ++ py x ++ ")"
py (Neg x) = "-" ++ py x ++ ""
py (Sqrt x) = "sqrt(" ++ py x ++ ")"
py (Rnd x) = "int(" ++ py x ++ ")"
py (Exp a b) = inf "**" a b
py (Mul a b) = inf "*" a b
py (Div a b) = inf "/" a b
py (IntDiv a b) = inf "//" a b
py (Mod a b) = inf "%" a b
py (Add a b) = inf "+" a b
py (Sub a b) = inf "-" a b
py (Number x) = show x
py Red = "r"
py Green = "g"
py Blue = "b"
py Arg = "n"

inf :: String -> Expr -> Expr -> String
inf op a b = py a ++ op ++ py b

toPython :: PixllVal -> String
toPython (Comment _) = "" -- We might do something with comments later
toPython (Array name pattern arr1 arr2) =
  "def " ++ name ++ "():\n" ++
  "    return Array(\"" ++ pattern ++ "\", " ++ arr1 ++ "(), " ++ arr2 ++ "())\n\n"
toPython (Transformation name setR setG setB) =
  "def " ++ name ++ "(gen, n):\n" ++
  "    def _" ++ name ++ "(c):\n" ++
  "        r, g, b = c\n" ++
  "        r = " ++ py setR ++ "\n" ++
  "        g = " ++ py setG ++ "\n" ++
  "        b = " ++ py setB ++ "\n" ++
  "        return (clamp(r), clamp(g), clamp(b))\n" ++
  "    gen.transform(_" ++ name ++ ")\n" ++
  "    return gen\n\n"

toPython (Procedure procedureName arrName transforms times speed) =
  "def " ++ procedureName ++ "(sh):\n" ++
  "    arr = " ++ arrName ++ "()\n" ++
  "    for _ in range(" ++ show times ++ "):\n" ++
  (concatMap transformToPython transforms) ++
  "        for i in all_pixels():\n" ++
  "            set_pixel(i, arr[i])\n" ++
  "        sh(" ++ show speed ++ ")\n\n"

transformToPython :: Transform -> String
transformToPython (Transform transformName arg) =
  "        arr = " ++ transformName ++ "(arr, " ++ show arg ++ ")\n"


-- Parsing
parseComment :: Parser PixllVal
parseComment = do
  char '#'
  contents <- many (noneOf "\n")
  spaces
  return $ Comment contents

parseIdentifier :: Parser String
parseIdentifier = many1 $ satisfy (\x -> isAlphaNum x || x == '_')

parseArray :: Parser PixllVal
parseArray = do
  string "array"
  many nonNewlineSpace
  name <- parseIdentifier
  spaces
  arr1 <- parseIdentifier
  spaces
  pattern <- many (oneOf "*-|")
  spaces
  arr2 <- parseIdentifier
  spaces
  return $ Array name pattern arr1 arr2

nonNewlineSpace :: Parser Char
nonNewlineSpace = oneOf " \t"

parseTransform :: Parser Transform
parseTransform = do
  many nonNewlineSpace
  name <- parseIdentifier
  many nonNewlineSpace
  arg <- decimal
  newline
  return $ Transform name arg

-- LOGIC:
-- x < x
-- x <= x
-- x > x
-- x >= x
-- x == x
-- x != x

-- not x

-- x and x

-- x or x

-- x ? x : x
--
parseOperatorExpr :: Parsec String () Expr
parseOperatorExpr = buildExpressionParser operators parseMathExpr

operators = 
  [
    [ Infix (char '^' <* many nonNewlineSpace >> return Exp) AssocRight 
    ]
  , [ Infix (char '*' <* many nonNewlineSpace >> return Mul) AssocLeft
    , Infix (string "//" <* many nonNewlineSpace >> return IntDiv) AssocLeft
    , Infix (char '/' <* many nonNewlineSpace >> return Div) AssocLeft
    , Infix (char '%' <* many nonNewlineSpace >> return Mod) AssocLeft
    ]
  , [ Infix (char '+' <* many nonNewlineSpace >> return Add) AssocLeft
    , Infix (char '-' <* many nonNewlineSpace >> return Sub) AssocLeft
    ]
  ]

parseMathExpr :: Parser Expr
parseMathExpr =
      Paren <$> between (char '(' <* many nonNewlineSpace) (char ')' <* many nonNewlineSpace) parseMathExpr
  <|> prefixExpr Neg (char '-') <* many nonNewlineSpace
  <|> prefixExpr Sqrt (string "sqrt") <* many nonNewlineSpace
  <|> prefixExpr Rnd (string "int") <* many nonNewlineSpace
  <|> Red <$ char 'r' <* many nonNewlineSpace
  <|> Green <$ char 'g' <* many nonNewlineSpace
  <|> Blue <$ char 'b' <* many nonNewlineSpace
  <|> Arg <$ char 'n' <* many nonNewlineSpace
  <|> Number . read <$> many1 digit <* many nonNewlineSpace

prefixExpr :: (Expr -> Expr) -> Parser a -> Parser Expr
prefixExpr n x = do
  x
  many nonNewlineSpace
  e <- parseMathExpr
  return $ n e

parseTransformation :: Parser PixllVal
parseTransformation = do
  string "transform"
  many nonNewlineSpace
  name <- parseIdentifier
  spaces
  r <- parseOperatorExpr
  char '\n'
  many nonNewlineSpace
  g <- parseOperatorExpr
  char '\n'
  many nonNewlineSpace
  b <- parseOperatorExpr
  spaces
  return $ Transformation name r g b

parseProcedure :: Parser PixllVal
parseProcedure = do
  name <- parseIdentifier
  optional nonNewlineSpace
  times <- option 20 decimal
  optional (char '@')
  speed <- option 0.2 (floating3 True)
  spaces
  char '>'
  spaces
  startingArr <- parseIdentifier
  newline
  transforms <- many parseTransform
  spaces
  return $ Procedure name startingArr transforms times speed

parseExpr :: Parser PixllVal
parseExpr =
      parseComment
  <|> parseArray
  <|> parseTransformation
  <|> parseProcedure

parseFile :: Parser [PixllVal]
parseFile = many parseExpr
