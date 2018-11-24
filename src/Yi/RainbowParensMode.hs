------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

------------------------------------------------------------------

module Yi.RainbowParensMode
  ( rainbowParensMode
  ) where

------------------------------------------------------------------

import Data.Foldable (asum)
import Data.Maybe (catMaybes, fromMaybe)

import Yi
  ( Mode(modeName, modeHL, modeGetStrokes)
  , importStyle, stringStyle, numberStyle, dataConstructorStyle
  )
import Yi.Mode.Common (fundamentalMode)
import Yi.Style
  ( Style
  , StyleName
  , UIStyle(UIStyle)
  , Color(RGB)
  , withFg
  )
import Yi.Syntax
  ( Point
  , Stroke
  , Span(Span)
  , Highlighter(SynHL)
  , Scanner(scanRun,scanInit)
  , ExtHL(ExtHL)
  )

import Text.Regex.Applicative (RE, psym, many, (=~))

------------------------------------------------------------------

-- [Section: Types]
-- ~~~~~~~~~~~~~~~~

-- | '(' or ')'
data Paren
  = Open  !Point
  | Close !Point

newtype Level = Level Int deriving (Eq, Num)

data Rainbow = Rainbow !Paren !Level
newtype RainbowStorage = RainbowStorage [Rainbow]

data CharLoc = CharLoc
  { _charLocPoint :: !Point
  , _charLocChar  :: !Char
  }

data RainbowColours
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet
  deriving (Enum)

------------------------------------------------------------------

-- [Section: Main]
-- ~~~~~~~~~~~~~~~

rainbowParensMode :: Mode RainbowStorage
rainbowParensMode = fundamentalMode
  { modeName = "rainbow paren"
  , modeHL   = ExtHL rainbowHighlighter
  , modeGetStrokes = \r _ _ _ -> rainbowGetStrokes r
  }

rainbowHighlighter :: Highlighter RainbowStorage RainbowStorage
rainbowHighlighter = SynHL
  (RainbowStorage [])
  (\scanner _point _oldTokens ->
       let initSt = scanInit scanner
           runner = scanRun scanner
           tupleToCharLoc !(!p, !c) = CharLoc p c
       in fromMaybe (RainbowStorage []) (tokenise $ map tupleToCharLoc (runner initSt))
  )
  (\tokens _windowRef -> tokens)
  (\_refToRegion tokens -> tokens)

rainbowGetStrokes :: RainbowStorage -> [Stroke]
rainbowGetStrokes (RainbowStorage tokens) = map
  ( \(Rainbow token level) ->
       let pos = tokenPoint token
       in Span pos (styleForLevel level) (pos + 1)
  ) tokens

------------------------------------------------------------------

-- [Section: Helpers]
-- ~~~~~~~~~~~~~~~~~~
tokenPoint :: Paren -> Point
tokenPoint (Open  p) = p
tokenPoint (Close p) = p

styleForLevel :: Level -> StyleName
styleForLevel (Level !l) = colours !! (l `rem` length colours)
  where
    colours :: [UIStyle -> Style]
    colours = map (\ !c _ -> withFg (colourToColor c)) [Red .. Violet]

colourToColor :: RainbowColours -> Color
colourToColor = \case
  Red    -> RGB 255 0 0
  Orange -> RGB 255 127 0
  Yellow -> RGB 255 255 0
  Green  -> RGB 0 255 0
  Blue   -> RGB 0 0 255
  Indigo -> RGB 75 0 130
  Violet -> RGB 148 0 211

------------------------------------------------------------------

-- [Section: Lexer/Tokeniser]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

tokenise :: [CharLoc] -> Maybe RainbowStorage
tokenise input = fmap assignLevels (input =~ lexTokens)

lexTokens :: RE CharLoc [Paren]
lexTokens = catMaybes <$> (many . asum) [ Just <$> lexToken, Nothing <$ lexIrrelevant ]

lexToken :: RE CharLoc Paren
lexToken = asum
  [ Open  . _charLocPoint  <$> psym ((== '(') . _charLocChar)
  , Close . _charLocPoint  <$> psym ((== ')') . _charLocChar)
  ]

isParen :: Char -> Bool
isParen !c = c == ')' || c == '('

lexIrrelevant :: RE CharLoc ()
lexIrrelevant = () <$ many (psym ((not . isParen) . _charLocChar))

-- This could be more efficient... (?)
-- It builds up the List backwards, then reverses it at the end.
assignLevels :: [Paren] -> RainbowStorage
assignLevels = RainbowStorage . go [] 0
  where
    go acc !_ [] = reverse acc
    go acc 0 (Close p : tokens) = go ((Rainbow (Close p) 0) : acc) 0 tokens
    go acc !level (t@(Open _) : tokens) =
      go ((Rainbow t level) : acc) (level + 1) tokens
    go acc !level (t@(Close _) : tokens) =
      go ((Rainbow t (level - 1)) : acc) (level - 1) tokens

{-
data SnocList where
  Nil  :: SnocList
  Cons :: SnocList -> Rainbow -> SnocList

snocToList :: SnocList -> [Rainbow]
snocToList Nil = []
snocToList (Cons xs x) = (snocToList xs) ++ [x]

(+++) :: SnocList -> Rainbow -> SnocList
(+++) = Cons

assignLevels' :: [Paren] -> RainbowStorage
assignLevels' = RainbowStorage . snocToList . go Nil 0
  where
    go acc !_ [] = acc
    go acc 0 (Close p : tokens) = go (acc +++ (Rainbow (Close p) 0)) 0 tokens
    go acc !level (t@(Open _) : tokens) =
      go (acc +++ (Rainbow t level)) (level + 1) tokens
    go acc !level (t@(Close _) : tokens) =
      go (acc +++ (Rainbow t (level - 1))) (level - 1) tokens
-}

------------------------------------------------------------------
