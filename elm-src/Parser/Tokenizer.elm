module Parser.Tokenizer(..) where


--------------------------------------------------------------------------------
-- EXTERNAL DEPENDENCIES
--------------------------------------------------------------------------------

import ElmTest exposing (..)
import Maybe exposing (Maybe(Just, Nothing))
import Regex exposing (..)
import String


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

type TokenType =    StartComment
                    | Word
                    | Whitespace
                    | Newline
                    | OpeningBracket
                    | ClosingBracket
                    | EqualsSign
                    | Semicolon
                    | Comma

type alias Chars = List Char
type alias TokenValue = String
type alias Token = (TokenType, TokenValue)
type alias Tokens = List Token
type alias TokenRecipe = (TokenType, String)
type alias RemainderString = String


--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

reservedCharTokenLookup : List ( TokenType, String )
reservedCharTokenLookup =
  [ (OpeningBracket, "\\(")
  , (ClosingBracket, "\\)")
  , (EqualsSign, "=")
  , (Semicolon, ";")
  , (Newline, "\n")
  , (Comma, ",")
  ]

specialSequences : List ( TokenType, String )
specialSequences =
    [ (StartComment, "//")
    ]

resevedRegexChars : String
resevedRegexChars = "()"

wordRegex : String
wordRegex =
    let
        reservedChars =
            List.foldl
              (\(_,c) s ->
                if
                  (String.contains c resevedRegexChars)
                then
                  -- Debug.log "foundReservedRegex" (s ++ "\\" ++ "c")
                  -- Debug.log "foundReservedRegex" (s ++ "\\" ++ "c")
                  s ++ c
                else
                  s ++ c
              )
              ""
              reservedCharTokenLookup
    in
        "[^" ++ reservedChars ++ "\\s" ++ "]+"

wildcards : List ( TokenType, String )
wildcards =
    [
        (Whitespace, "(\\s)+"),
        (Word, wordRegex)
    ]

tokenizerGrammar : List TokenRecipe
tokenizerGrammar = specialSequences ++ reservedCharTokenLookup ++ wildcards


consumeToken : TokenRecipe -> String   ->   Maybe (Token, RemainderString)
consumeToken (tokenType, regexString) s =
    let
        regexString' = "^" ++ regexString
        regex' = regex (Debug.log "regexString" regexString')
    in
        case find (AtMost 1) regex' s of
            [] ->
                Nothing
            match::_ ->
                let
                    token = (tokenType, match.match)
                    remainder = replace (AtMost 1) regex' (\_ -> "") s
                in
                    Just (token, remainder)


consumeFirstTokenMatch   :   (List TokenRecipe) -> String    ->   Maybe (Token, RemainderString)
consumeFirstTokenMatch tokenRecipes s =
    case tokenRecipes of
        [] -> Nothing
        tokenRecipe::tailTokenRecipes ->
            case consumeToken tokenRecipe s of
                Nothing ->
                    consumeFirstTokenMatch tailTokenRecipes s
                Just result ->
                    Just result

tokenize   :  String   ->   List Token
tokenize s =
    let
        _ = Debug.log "grammar" tokenizerGrammar
        tokenize' : List Token -> String   ->   List Token
        tokenize' accTokens remainderString =
            case consumeFirstTokenMatch tokenizerGrammar remainderString of
                Nothing ->
                    accTokens
                Just (token, remainderRemainderString) ->
                    tokenize' (accTokens ++ [token]) remainderRemainderString

    in
        tokenize' [] s



--------------------------------------------------------------------------------
-- TESTS
--------------------------------------------------------------------------------

tests : Test
tests = suite "Tokenizer.elm"
    [
        -- test "consumeToken"
        -- ( assertEqual
        --     (Just <| ((ExclamationMark, "!"), "hello"))
        --     (consumeToken (ExclamationMark, "!") "!hello")
        -- )
      test "tokenize" (
          assertEqual
              [(Word, "helloz")]
              -- (tokenize "//-\nwah4(fr) = 4;")
              (tokenize """
//-------------------------- wah4(fr) -------------------------------
// Wah effect, 4th order
// USAGE: wah4(fr), where fr = resonance frequency in Hz
// REFERENCE "https://ccrma.stanford.edu/~jos/pasp/vegf.html";
//
wah4(fr) = 4*moog_vcf((3.2/4),fr:smooth(0.999));
              """)
      )
--     , test "consumeWhitespace"
--         (assertEqual
--           (Just <| ((Whitespace, "   "), "hello"))
--           (consumeToken (Whitespace, "(\\s)+") "   hello")
--         )
--     --         ,
--     --         test "consumeFirstTokenMatch (!)" (
--     --             assertEqual
--     --                 (Just
--     --                     ((ExclamationMark, "!"), "x")
--     --                 )
--     --                 (consumeFirstTokenMatch tokenizerGrammar "!x")
--     --         )
--     , test "consumeFirstTokenMatch (!)"
--         ( assertEqual
--             (Just ((OpeningComment, "<!--"), " hello"))
--             (consumeFirstTokenMatch tokenizerGrammar "<!-- hello")
--         )
--     , test "consumeWhitespace"
--         ( assertEqual
--             (Just ((Whitespace, "   "), "hello") )
--             (consumeFirstTokenMatch tokenizerGrammar "   hello")
--         )
--     , test "consumeWord"
--         ( assertEqual
--             (Just ((Word, "one"), ">two") )
--             (consumeFirstTokenMatch tokenizerGrammar "one>two")
--         )
    --         ,
    --         test "tokenize" (
    --             assertEqual
    --                 [(Word, "hello"), (ExclamationMark, "!")]
    --                 (tokenize "hello!")
    --         )
    ]
--
--
main =
    elementRunner tests
