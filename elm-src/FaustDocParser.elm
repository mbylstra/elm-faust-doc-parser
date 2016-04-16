module FaustDocParser where
  -- ( xhtmlToRawAst
  -- , main
  -- , tests
  -- )
  -- where





--------------------------------------------------------------------------------
-- EXTERNAL DEPENDENCIES
--------------------------------------------------------------------------------

import ElmTest exposing (..)


--------------------------------------------------------------------------------
-- INTERNAL DEPENDENCIES
--------------------------------------------------------------------------------

import Parser.Tokenizer exposing (..)
import Parser.Parser exposing
  ( ParseResult
    ( ParseMatchesReturnsResult
    , ParseMatchesReturnsNothing
    , ParseDoesNotMatch
    )
  , AstNode (LabelledAstNode, UnlabelledAstNode)
  , AstNodeValue (AstLeaf, AstChildren)
  , createParseSequenceFunction
  , createOptionallyParseMultipleFunction
  , labelled
  , createParseAtLeastOneFunction
  , createParseAnyFunction
  , ignore
  , createParseTokenIgnoreFunction
  , ParseFunction
  , optional
  )
import Parser.ParserHelpers exposing (..)


-- example:
-- //-------------------------- wah4(fr) -------------------------------
-- // Wah effect, 4th order
-- // USAGE: wah4(fr), where fr = resonance frequency in Hz
-- // REFERENCE "https://ccrma.stanford.edu/~jos/pasp/vegf.html";
-- //
-- wah4(fr) = 4*moog_vcf((3.2/4),fr:smooth(0.999));

-- [(Word,"hello")]; got: [(StartComment,"//"),(Word,"-"),(Newline,"\n"),(Word,"wah4"),(OpeningBracket,"("),(Word,"fr"),(ClosingBracket,")"),(Whitespace," "),(EqualsSign,"="),(Whitespace," "),(Word,"4"),(Semicolon,";")]

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

parse : ParseFunction
parse =
    createParseAtLeastOneFunction
    <| createParseAnyFunction
        [ parseComment
        , parseNewline
        , parseFunction
        ]

parseComment : ParseFunction
parseComment =
  createParseSequenceFunction
    [ optional parseWhitespace
    , parseStartComment
    , parseAnythingButNewline
    , parseNewline
    ]
  |> labelled "COMMENT"

parseAnythingButNewline : ParseFunction
parseAnythingButNewline =
  createParseAtLeastOneFunction
    <| createParseAnyFunction
        [ parseWord
        , parseWhitespace
        , parseEqualsSign
        , parseOpeningBracket
        , parseClosingBracket
        , parseSemicolon
        , parseComma
        ]


parseFunction : ParseFunction
parseFunction =
  labelled "FUNCTION" <|
    createParseSequenceFunction
      [ optional parseWhitespace
      , labelled "FUNCTION_NAME" parseWord
      , optional parseWhitespace
      , optional parseFunctionArgs
      , optional parseWhitespace
      , parseEqualsSign
      , optional parseWhitespace
      , parseAnythingButSemicolon
      , parseSemicolon
      , optional parseWhitespace
      ]

parseFunctionArgs =
  labelled "FUNCTION_ARGS" <|
    createParseSequenceFunction
      [ parseOpeningBracket
      , createParseAtLeastOneFunction
        <| createParseSequenceFunction
          [ optional parseWhitespace
          , labelled "FUNCTION_ARGUMENT" parseWord
          , optional parseWhitespace
          , optional parseComma
          , optional parseWhitespace
          ]
      , parseClosingBracket
      ]


    -- parseAtLeastOne
    --  parseSquence(word, optional comma)

parseAnythingButSemicolon : ParseFunction
parseAnythingButSemicolon =
  createParseAtLeastOneFunction
    <| createParseAnyFunction
        [ parseWord
        , parseWhitespace
        , parseEqualsSign
        , parseOpeningBracket
        , parseClosingBracket
        , parseComma
        ]


--------------------------------------------------------------------------------
-- TESTS
--------------------------------------------------------------------------------

dummyResult =
  (ParseMatchesReturnsResult <| UnlabelledAstNode <| AstChildren
    []
  , []
  )

tests = suite ""
  [
--     test ""
--     ( assertEqual
--         dummyResult
--         ( parse
--             ( tokenize
--                 """
-- //-------------------------- wah4(fr) -------------------------------
-- // Wah effect, 4th order
-- // USAGE: wah4(fr), where fr = resonance frequency in Hz
-- // REFERENCE "https://ccrma.stanford.edu/~jos/pasp/vegf.html";
-- //
-- wah4(fr) = 4*moog_vcf((3.2/4),fr:smooth(0.999));
--                 """
--             )
--         )
--       )
  -- , test ""
  --   ( assertEqual
  --       dummyResult
  --       ( parseAnythingButSemicolon
  --         ( tokenize
  --             """a = 1;"""
  --         )
  --       )
  --     )
    -- test ""
    -- ( assertEqual
    --     dummyResult
    --     ( parseFunction
    --       ( tokenize
    --           -- """ x = 1;  \n"""
    --           """wah4(fr) = 4*moog_vcf((3.2/4),fr:smooth(0.999));
    --           """
    --       )
    --     )
    --   )
  -- , test ""
  --   ( assertEqual
  --       dummyResult
  --       ( parseFunctionArgs
  --         ( tokenize
  --             """( a,b  , c)"""
  --         )
  --       )
  --     )
    test ""
    ( assertEqual
        dummyResult
        ( parse
          ( tokenize
              -- """ x = 1;  \n"""
            """
            //-----------------------------------------------
            //  DELAY LINE
            //-----------------------------------------------
            frac(n)                 = n-int(n);
            index(n)                = &(n-1) ~ +(1);                // n = 2**i
            //delay(n,d,x)  = rwtable(n, 0.0, index(n), x, (index(n)-int(d)) & (n-1));
            delay(n,d,x)    = x@(int(d)&(n-1));
            fdelay(n,d,x)   = delay(n,int(d),x)*(1 - frac(d)) + delay(n,int(d)+1,x)*frac(d);
            """
          )
        )
    )
  ]
--
main =
    elementRunner tests
