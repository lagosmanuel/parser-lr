module Main where

-- Context Free Grammar Types and Functions
newtype Terminal    = Terminal String deriving (Eq)
newtype Nonterminal = Nonterminal String deriving (Eq)
newtype Production  = Production (Nonterminal, [Either Terminal Nonterminal])

data CFG = CFG {
    nonterminals :: [Nonterminal],
    terminals    :: [Terminal],
    rules        :: [Production],
    start        :: Nonterminal
}

instance Show Terminal where
    show (Terminal string) = string

instance Show Nonterminal where
    show (Nonterminal string) = string

instance Show Production where
    show (Production (lval, rval)) = show lval ++ " -> " ++ showRVal rval 

showRVal :: [Either Terminal Nonterminal] -> String
showRVal []                         = ""
showRVal ((Left terminal):rest)     = show terminal ++ showRVal rest
showRVal ((Right nonterminal):rest) = show nonterminal ++ showRVal rest

createNonterminals :: [String] -> [Nonterminal]
createNonterminals = map Nonterminal

createTerminals :: [String] -> [Terminal]
createTerminals = map Terminal

createProductions :: [(Nonterminal, [Either Terminal Nonterminal])] -> [Production]
createProductions = map Production 

addProduction :: CFG -> Production -> CFG
addProduction cfg production = 
    CFG (nonterminals cfg) (terminals cfg) (production:rules cfg) (start cfg)

getSymbol :: Production -> Int -> Maybe (Either Terminal Nonterminal)
getSymbol (Production (_, symbols)) index = 
    if index >= 0 && index <= length symbols 
    then Just (symbols !! index) 
    else Nothing

filterProductions :: CFG -> Nonterminal -> [Production]
filterProductions cfg nonterminal = 
    [Production prod | Production prod <- rules cfg, nonterminal == fst prod]

hasLVal :: [Production] -> Nonterminal -> Bool
hasLVal [] _ = False
hasLVal ((Production prod):prods) nonterminal = let nonterm = fst prod
                                                in nonterminal == nonterm 
                                                || hasLVal prods nonterminal

-- Parser Types and Functions
newtype Item  = Item (Production, Int)
newtype State = State [Item]

instance Show Item where
    show (Item (prod, index)) = "(" ++ show prod ++ ", " ++ show index ++ ")"

instance Show State where
    show (State []) = ""
    show (State items) = show items

addItem :: State -> Item -> State
addItem (State state) item = State (item:state)

getClosure :: CFG -> State -> State
getClosure _ (State []) = State []
getClosure cfg state    = 
  let
    State (item:items)        = state
    Item  (production, index) = item
    symbol                    = getSymbol production index
  in 
    case symbol of
      Just (Left _)            -> addItem (getClosure cfg (State items)) item
      Just (Right nonterminal) -> addItem (getClosure cfg (State newState)) item where 
        newProductions = filterProductions cfg nonterminal
        newItems       = map (\prod -> Item (prod, 0)) newProductions  
        newState       = if hasLVal (rules cfg) nonterminal 
                         then items ++ newItems else items
      Nothing                  -> State []

-- Test Case 1

n :: [Nonterminal]
n = createNonterminals ["S", "M", "L", "J", "H"]

t :: [Terminal]
t = createTerminals ["a", "b", "c"]

s :: Nonterminal
s = Nonterminal "S"

p :: [Production]
p = createProductions [
    (Nonterminal "S", [Right (Nonterminal "M")]),
    (Nonterminal "M", [Right (Nonterminal "L"), Left (Terminal "a")]),
    (Nonterminal "L", [Right (Nonterminal "J")]),
    (Nonterminal "J", [Left (Terminal "b"), Right (Nonterminal "H")]),
    (Nonterminal "H", [Left (Terminal "c")])
  ]

myCfg :: CFG
myCfg = CFG n t p s

myState :: State
myState = let productions = filterProductions myCfg (start myCfg)
          in aux productions 
          where
            aux [] = State []
            aux (Production prod:prods) = addItem (aux prods) (Item (Production prod, 0))

myClosure :: State
myClosure = getClosure myCfg myState

main :: IO ()
main = putStrLn "Hello, Haskell!"
