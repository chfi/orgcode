import Protolude

exampleEmpty :: [Text]
exampleEmpty =
  [ "hello this is some text"
  , "123 so is this!!"
  , "in fact there is no code here."
  ]

exampleCode :: [Text]
exampleCode =
  [ "hello this is some text"
  , "here comes some code!!"
  , "#+BEGIN_SRC purescript"
  , "clickAnnGlyphs :: forall f g a."
  , "                  Filterable f"
  , "               => Functor g"
  , "               => Eq (g Boolean)"
  , "               => f (g (Glyph a))"
  , "               -> Point"
  , "               -> f (g (Glyph a))"
  , "clickAnnGlyphs gs p ="
  , "     filter (\\x -> map pred x == map (const true) x) gs"
  , "  where pred = \\g -> unwrap (glyphBounds g) p"
  , "#+END_SRC"
  , "that was a fair bit of code"
  ]

exampleCode2 :: [Text]
exampleCode2 = exampleCode <>
  [ "and here comes some more"
  , "soon!!"
  , "#+BEGIN_SRC javascript"
  , "if (g.glyphs) {"
  , "    return glyphLookup(g.glyphs, rx, ry, matches);"
  , "} else if (g.glyph) {"
  , "    return glyphLookup([g.glyph], rx, ry, matches);"
  , "} else {"
  , "    return matches;"
  , "}"
  , "#+END_SRC"
  , "but that is all"
  ]

exHeader :: [Text]
exHeader = [":name", "hello", ":output", "farts"]

badHeader :: [Text]
badHeader = [":name", "hello", "output", ":nope", "yes"]

badHeader2 :: [Text]
badHeader2 = ["weird!", ":name", "hello", "output", ":nope", "yes"]

main :: IO ()
main = putStrLn ("Test suite not yet implemented" :: Text)
