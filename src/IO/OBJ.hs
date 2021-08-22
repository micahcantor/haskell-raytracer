module IO.OBJ where

import Constants (defaultGroup, triangle)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Shape (addChildren)
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser, parseFromFile)
import Types (Point (..), Shape, children)

data ObjVertex = Vertex Double Double Double
  deriving (Show, Eq)

newtype ObjFace = Face [Int]
  deriving (Show, Eq)

data ObjLine = V ObjVertex | F ObjFace | G
  deriving (Show, Eq)

data ParseResult = ParseResult
  { vertices :: Map Int Point,
    groups :: [Shape]
  }
  deriving (Show, Eq)

whitespace = skipMany space

padded = between whitespace whitespace

eol :: Parser String
eol =
  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "EOL"

int :: Parser Int
int = do
  value <- padded (many1 digit)
  return (read value)

double :: Parser Double
double = do
  value <- padded $ many1 (digit <|> oneOf ['.', '-'])
  return (read value)

vertex :: Parser ObjLine
vertex = do
  char 'v'
  [x, y, z] <- count 3 double
  return (V (Vertex x y z))

face :: Parser ObjLine
face = do
  char 'f'
  vertices <- many1 int
  return (F (Face vertices))

group :: Parser ObjLine
group = do
  padded (char 'g')
  manyTill anyChar eol
  return G

objLines :: Parser [ObjLine]
objLines =
  map head <$> many1 ((vertex <|> face <|> group) `sepBy1` eol)

triangulate :: [Point] -> [Shape]
triangulate [x, y, z] = [triangle x y z]
triangulate (a : vertices) =
  let adjacent = zip vertices (tail vertices)
   in [triangle a b c | (b, c) <- adjacent]

parseObj :: [ObjLine] -> ParseResult
parseObj lines = evalState loop (lines, 0, Map.empty, [defaultGroup])
  where
    toPoint (Vertex x y z) = Point x y z
    loop :: State ([ObjLine], Int, Map Int Point, [Shape]) ParseResult
    loop = do
      (lines, vtxCount, vertices, groups) <- get
      case lines of
        [] -> do
          return (ParseResult vertices groups)
        l : ls -> do
          let g : gs = groups
          case l of
            V vertex -> do
              let newVertices = Map.insert (vtxCount + 1) (toPoint vertex) vertices
              put (ls, vtxCount + 1, newVertices, groups)
            F (Face ixs) -> do
              let points = [vertices ! i | i <- ixs]
              let triangles = reverse (triangulate points)
              let g' = fst (addChildren g triangles)
              put (ls, vtxCount, vertices, g' : gs)
            G ->
              put (ls, vtxCount, vertices, defaultGroup : groups)
          loop

objToGroup :: ParseResult -> Shape
objToGroup ParseResult {groups} =
  let defaultGroup = last groups
   in fst (addChildren defaultGroup (init groups))

parseObjFile :: FilePath -> IO ParseResult
parseObjFile source = do
  parsed <- parseFromFile objLines source
  case parsed of
    Left err -> error (show err)
    Right tokens -> return (parseObj tokens)