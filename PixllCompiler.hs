import Text.ParserCombinators.Parsec
import Data.List
import Data.Char (isAlphaNum)
import System.IO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  case parse parseFile "PixllParser" input of
    Left err -> hPutStrLn stderr $ show err
    Right val -> mapM_ putStrLn . lines $ compileToPython val

type LightPattern = String
type ArrayName = String
type ProcedureName = String
type TransformName = String
type TransformationName = String
type PythonCode = String

data Transform = Transform TransformName Int
  deriving Show

data PixllVal = Array ArrayName LightPattern ArrayName ArrayName 
              | Comment String
              | Procedure ProcedureName ArrayName [Transform]
              | Transformation TransformationName PythonCode PythonCode PythonCode

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
  "        for t in range(20):\n" ++
  "            proc(t)\n" ++
  "            sh(0.2)\n"

isProcedure :: PixllVal -> Bool
isProcedure (Procedure _ _ _) = True
isProcedure _ = False

procedureToString :: PixllVal -> String
procedureToString (Procedure name _ _) = name
procedureToString _ = error "procedureToString can only convert procedures"

toPython :: PixllVal -> String
toPython (Comment _) = "" -- We might do something with comments later
toPython (Array name pattern arr1 arr2) =
  "def " ++ name ++ "():\n" ++
  "    return Array(\"" ++ pattern ++ "\", " ++ arr1 ++ "(), " ++ arr2 ++ "())\n\n"
toPython (Transformation name setR setG setB) =
  "def " ++ name ++ "(gen, n):\n" ++
  "    def _" ++ name ++ "(c):\n" ++
  "        r, g, b = c\n" ++
  "        r = " ++ setR ++ "\n" ++
  "        g = " ++ setG ++ "\n" ++
  "        b = " ++ setB ++ "\n" ++
  "        return (clamp(r), clamp(g), clamp(b))\n" ++
  "    gen.transform(_" ++ name ++ ")\n" ++
  "    return gen\n\n"

toPython (Procedure procedureName arrName transforms) =
  "def " ++ procedureName ++ "(t):\n" ++
  "    arr = " ++ arrName ++ "()\n" ++
  "    for _ in range(t):\n" ++
  (concatMap transformToPython transforms) ++
  "        for i in all_pixels():\n" ++
  "            set_pixel(i, arr[i])\n\n"

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
parseIdentifier = many1 (satisfy (\x -> isAlphaNum x || x == '_'))

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
  arg <- many1 digit
  newline
  return $ Transform name (read arg)

parseTransformation :: Parser PixllVal
parseTransformation = do
  string "transform"
  many nonNewlineSpace
  name <- parseIdentifier
  spaces
  r <- many (noneOf "\n")
  spaces
  g <- many (noneOf "\n")
  spaces
  b <- many (noneOf "\n")
  spaces
  return $ Transformation name r g b

parseProcedure :: Parser PixllVal
parseProcedure = do
  name <- parseIdentifier
  spaces
  char '>'
  spaces
  startingArr <- parseIdentifier
  newline
  transforms <- many parseTransform
  spaces
  return $ Procedure name startingArr transforms

parseExpr :: Parser PixllVal
parseExpr =
      parseComment
  <|> parseArray
  <|> parseTransformation
  <|> parseProcedure

parseFile :: Parser [PixllVal]
parseFile = many parseExpr
