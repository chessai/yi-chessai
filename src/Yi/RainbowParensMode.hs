------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

------------------------------------------------------------------

module Yi.RainbowParensMode
  ( rainbowParensMode
  ) where

------------------------------------------------------------------

import Data.Foldable (asum)
import Data.Maybe (catMaybes, fromMaybe)

import Yi
  ( StyleName
  , Mode(modeName, modeHL, modeGetStrokes)
  , importStyle, stringStyle, numberStyle, dataConstructorStyle
  )
import Yi.Mode.Common (fundamentalMode)
import Yi.Syntax
  ( Point
  , Stroke
  , Span(Span)
  , Highlighter(SynHL)
  , Scanner(scanRun,scanInit)
  , ExtHL(ExtHL))

import Text.Regex.Applicative (RE, psym, many, (=~))

------------------------------------------------------------------

-- [Section: Types]
-- ~~~~~~~~~~~~~~~~

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
    colours =
      [ importStyle
      , stringStyle
      , numberStyle
      , dataConstructorStyle
      ]

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
