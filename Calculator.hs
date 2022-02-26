module Calculator where

-- Chapter 4: Reverse Polish calculator 

import Text.Read (readEither)

data Expr =
      Lit Int
    | Sub Expr Expr
    | Add Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving Show

eval :: Expr -> Either String Int
eval expr =
    case expr of
        Lit num ->  Right num
        Add arg1 arg2 -> eval' (+) arg1 arg2
        Sub arg1 arg2 -> eval' (-) arg1 arg2
        Mul arg1 arg2 -> eval' (*) arg1 arg2
        Div arg1 arg2 -> if eval arg2 == (Right 0)
                            then (Left "Can't divide by 0")
                            else eval' div arg1 arg2
    where
        eval' op arg1 arg2 = 
            case eval arg1 of
                Left err -> Left err
                Right arg1' -> case eval arg2 of
                    Left err -> Left err
                    Right arg2' -> Right $ op arg1' arg2'

parse :: String -> Either String Expr
parse str =
    case parse' (words str) of
        Left err -> Left err
        Right (e, []) -> Right e
        Right (_, rest) -> Left $ "Found extra tokens: " <> (unwords rest)

    where
        parse' :: [String] -> Either String (Expr, [String])
        parse' [] = Left "Unexpected end of expression"
        parse' (token:rest) =
            case token of
                "+" -> parseBinary Add rest
                "-" -> parseBinary Sub rest
                "*" -> parseBinary Mul rest
                "/" -> parseBinary Div rest
                lit ->
                    case readEither lit of
                        Left err -> Left err
                        Right lit' -> Right (Lit lit', rest)


        parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
        parseBinary exprConstructor args =
            case parse' args of
                Left err -> Left err
                Right (firstArg, rest') ->
                    case parse' rest' of
                        Left err -> Left err
                        Right (secondArg, rest'') -> Right $ (exprConstructor firstArg secondArg, rest'')


run :: String -> String
run expr =
    case parse expr of
        Left err -> "Error: " <> err
        Right expr' ->
            case eval expr' of
                Left err -> "Error: " <> err
                Right expr'' ->
                    let answer = show expr''
                    in "The answer is: " <> answer


prettyPrint :: Expr -> String
prettyPrint expr =
    case expr of
        Lit num ->  show num
        Add arg1 arg2 -> prettyPrint arg1 <> " + " <> prettyPrint arg2
        Sub arg1 arg2 -> prettyPrint arg1 <> " - " <> prettyPrint arg2
        Mul arg1 arg2 -> prettyPrint arg1 <> " * " <> prettyPrint arg2
        Div arg1 arg2 -> prettyPrint arg1 <> " / " <> prettyPrint arg2


