module UI.SyntaxHighlight (highlightLine) where

import qualified Graphics.Vty as Vty
import Graphics.Vty (Image, Attr, defAttr, string, horizCat, withForeColor)
import Core.Types
import qualified Data.Text as T
import qualified Data.Char as Char

highlightLine :: Syntax -> T.Text -> Image
highlightLine PlainText txt = text' defAttr txt
highlightLine Haskell txt   = highlightHaskell txt
highlightLine Python txt    = highlightPython txt

text' :: Attr -> T.Text -> Image
text' attr = Vty.string attr . T.unpack

highlightHaskell :: T.Text -> Image
highlightHaskell txt =
    Vty.horizCat $ map highlightToken (tokenizeHaskell txt)

highlightPython :: T.Text -> Image
highlightPython txt =
    Vty.horizCat $ map highlightToken (tokenizePython txt)

highlightToken :: (TokenType, T.Text) -> Image
highlightToken (ttype, ttext) =
    text' (tokenAttr ttype) ttext

data TokenType = Keyword | Identifier | Symbol | Literal | Comment | Operator

tokenAttr :: TokenType -> Attr
tokenAttr Keyword    = defAttr `withForeColor` Vty.yellow
tokenAttr Identifier = defAttr
tokenAttr Symbol     = defAttr `withForeColor` Vty.magenta
tokenAttr Literal    = defAttr `withForeColor` Vty.green
tokenAttr Comment    = defAttr `withForeColor` Vty.brightBlue
tokenAttr Operator   = defAttr `withForeColor` Vty.red

tokenizeHaskell :: T.Text -> [(TokenType, T.Text)]
tokenizeHaskell = tokenizeGeneric haskellKeywords

tokenizePython :: T.Text -> [(TokenType, T.Text)]
tokenizePython = tokenizeGeneric pythonKeywords

tokenizeGeneric :: [T.Text] -> T.Text -> [(TokenType, T.Text)]
tokenizeGeneric keywords line = go (T.unpack line) []
  where
    go [] acc = reverse acc
    go str acc
        | take 2 str == "--" = (Comment, T.pack str) : acc
        | Char.isSpace (head str) =
            let (spaces, rest) = span Char.isSpace str
            in go rest ((Identifier, T.pack spaces) : acc)
        | isSymbol (head str) =
            let (symbols, rest) = span isSymbol str
            in go rest ((Symbol, T.pack symbols) : acc)
        | isOperator (head str) =
            let (ops, rest) = span isOperator str
            in go rest ((Operator, T.pack ops) : acc)
        | isDigit (head str) =
            let (digits, rest) = span isDigit str
            in go rest ((Literal, T.pack digits) : acc)
        | otherwise =
            let (word, rest) = span isAlphaNum str
                tokenType = if T.pack word `elem` keywords then Keyword else Identifier
            in go rest ((tokenType, T.pack word) : acc)

    isSymbol c = c `elem` ("(){}[],.;" :: String)
    isOperator c = c `elem` ("+-*/=<>&|^~:!" :: String)
    isDigit = Char.isDigit
    isAlphaNum = Char.isAlphaNum

haskellKeywords :: [T.Text]
haskellKeywords =
    map T.pack ["case", "class", "data", "default", "deriving", "do", "else"
               , "if", "import", "in", "infix", "infixl", "infixr", "instance"
               , "let", "module", "newtype", "of", "then", "type", "where"]

pythonKeywords :: [T.Text]
pythonKeywords =
    map T.pack ["def", "class", "if", "elif", "else", "try", "except", "finally"
               , "for", "while", "break", "continue", "return", "import", "from"
               , "as", "pass", "with", "lambda", "yield", "global", "nonlocal"]
