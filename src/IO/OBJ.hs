module IO.OBJ where

import Constants (defaultGroup, triangle)
import Data.Either (fromRight)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Shape (addChildren)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Types (Point (..), Shape, children)

data ObjVertex = Vertex Double Double Double
  deriving (Show, Eq)

newtype ObjFace = Face [Int]
  deriving (Show, Eq)

data ObjLine = V ObjVertex | F ObjFace | G
  deriving (Show, Eq)

data ObjGroup = ObjGroup
  { vertices :: Map Int ObjVertex,
    faces :: [ObjFace],
    vtxCount :: Int
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

parseGroups :: [ObjLine] -> [ObjGroup]
parseGroups = go [empty]
  where
    empty = ObjGroup {vertices = Map.empty, faces = [], vtxCount = 0}
    go groups [] = map (\g -> g { faces = reverse (faces g) }) groups
    go (g : gs) (line : lines) =
      case line of
        V vertex ->
          let newVertices = Map.insert (vtxCount g + 1) vertex (vertices g)
              g' = g {vertices = newVertices, vtxCount = vtxCount g + 1}
           in go (g' : gs) lines
        F face ->
          let g' = g {faces = face : faces g}
           in go (g' : gs) lines
        G -> go (empty : g : gs) lines

triangulate :: [Point] -> [Shape]
triangulate [x, y, z] = [triangle x y z]
triangulate (a : vertices) =
  let adjacent = zip vertices (tail vertices)
   in map (uncurry (triangle a)) adjacent

toObj :: [ObjGroup] -> Shape
toObj objGroups = fst (addChildren defaultGroup children)
  where
    children = concatMap f objGroups
    f ObjGroup {vertices, faces} =
      let vtxToPoint (Vertex x y z) = Point x y z
          triangulateFace (Face ixs) =
            let points = map (vtxToPoint . (vertices !)) ixs
             in triangulate points
       in concatMap triangulateFace faces

parseObj :: FilePath -> IO Shape
parseObj source = do
  tokens <- parseFromFile objLines source
  let groups = parseGroups (fromRight [] tokens)
  print groups
  return (toObj groups)