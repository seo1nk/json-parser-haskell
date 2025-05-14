{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- required
import Control.Applicative (Alternative (..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
-- show
import Data.Bool (Bool (False, True))
import qualified Data.ByteString as B (concatMap, intercalate)
import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace, ord)
import Data.Functor (($>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Negative, Positive)

{-
  JSON Data Type
-}
data JValue
  = JNull
  | JBool Bool
  | JString String
  | JNumber {int :: Integer, frac :: [Int], exponent :: Integer} -- タプル
  | JArray [JValue] -- 再帰
  | JObject [(String, JValue)] -- 再帰
  deriving (Eq, Generic)

{-
  Parser
  パーサーがパースに成功すれば残りの入力とパースした出力を返すが、失敗であれば何も返さない
-}
newtype Parser i o
  = Parser {runParser :: i -> Maybe (i, o)}

{-
  Char Parser
  char1パーサーは、指定された文字と入力文字列をマッチングして、入力が指定された文字で始まる場合に成功する。

  (x:xs) -> cons 演算子 `:` によるリストの分解で、x は先頭の文字、xs は残りの文字列となる。
  $ は関数適用演算子で、右側の式全体に左側の関数を適用する。
  \case はラムダケース式で、ひとつの要素に対する case 式の簡略化記法。
-}
char1 :: Char -> Parser String Char
char1 c = Parser $ \case
  (x : xs) | x == c -> Just (xs, x)
  _ -> Nothing

{-
  char1パーサーを、一般化したもの。

  Parser [a] a -> 入力リスト型 [a] を解析し、要素 a を返すパーサー。
  | predicate x -> ガード式で、先頭要素 x が条件 predicate を満たしているかを確認。
-}
satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
  (x : xs) | predicate x -> Just (xs, x)
  _ -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

{-
  Digit Parser

  \i -> 引数iを受け取るラムダ式。
  isDigit を使って0-9の文字を解析する。
  i' は、i を処理した後の残りの入力を示す。Haskellでは派生した値を慣習的に「'」をつけた変数で表現する。
  digitToInt o -> パースした結果を整数値にして返す。
-}
digit1 :: Parser String Int
digit1 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing -> Nothing
  Just (i', o) -> Just (i', digitToInt o)

{-
  digit1のリファクタ
-}
digit2 :: Parser String Int
digit2 = Parser $ \i -> case runParser (satisfy isDigit) i of
  Nothing -> Nothing
  Just (i', o) -> Just . fmap digitToInt $ (i', o)

digit3 :: Parser String Int
digit3 = Parser $ \i -> fmap (fmap digitToInt) . runParser (satisfy isDigit) $ i

{-
  digit1のリファクタ
  Functor になる
-}
instance Functor (Parser i) where
  fmap f parser = Parser $ fmap (fmap f) . runParser parser

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

{-
  String Parser
-}
string1 :: String -> Parser String String
string1 s = case s of
  "" -> Parser $ \i -> Just (i, "")
  (c : cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing -> Nothing
    Just (rest, _) -> case runParser (string1 cs) rest of
      Nothing -> Nothing
      Just (rest', _) -> Just (rest', c : cs)

{-
  string1 のリファクタ
-}
string2 :: String -> Parser String String
string2 s = case s of
  "" -> Parser $ pure . (,"")
  (c : cs) -> Parser $ \i -> case runParser (char c) i of
    Nothing -> Nothing
    Just (rest, c) -> fmap (c :) <$> runParser (string2 cs) rest

{-
  string1 のリファクタ
  Applicative になる
-}
instance Applicative (Parser i) where
  pure x = Parser $ pure . (,x)
  pf <*> po = Parser $ \input -> case runParser pf input of
    Nothing -> Nothing
    Just (rest, f) -> fmap f <$> runParser po rest

string :: String -> Parser String String
string "" = pure ""
string (c : cs) = (:) <$> char c <*> string cs

{-
  JNull Parser
  単なる文字列「null」の文字列パーサー
-}
jNull :: Parser String JValue
jNull = string "null" $> JNull

{-
  JBool Parser
  文字列「true」のパースをして、失敗したら「false」のパースをする（Backtrackingと呼ばれる）
  Alternative を使うと単純に実装できる
-}
instance Alternative (Parser i) where
  empty = Parser $ const empty
  p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

jBool :: Parser String JValue
jBool =
  string "true"
    $> JBool True
      <|> string "false"
    $> JBool False

{-
  JString Parser / 一文字ずつ

  JSONの文字列にはエスケープ文字があります。
  エスケープの選択肢すべてを記述します。記述の順番も大事で、注目すべきものは最後に書く。

  Applicative型クラスの`*>`を使うと、左側でパーサーが成功したらパースされた値を捨てて、右側でパーサーを実行することができる。
-}
jsonChar :: Parser String Char
jsonChar =
  string "\\\""
    $> '"'
      <|> string "\\\\"
    $> '\\'
      <|> string "\\/"
    $> '/'
      <|> string "\\b"
    $> '\b'
      <|> string "\\f"
    $> '\f'
      <|> string "\\n"
    $> '\n'
      <|> string "\\r"
    $> '\r'
      <|> string "\\t"
    $> '\t'
      <|> unicodeChar
      <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
  where
    unicodeChar =
      chr . fromIntegral . digitsToNumber 16 0
        <$> (string "\\u" *> replicateM 4 hexDigit)

    hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base =
  foldl (\num d -> num * fromIntegral base + fromIntegral d)

{-
  JString Parser / サロゲート表現対応

  JSONでの文字列は単なる文字のリストではなく、拡張文字をエスケープするために、UTF-16サロゲートペアをエンコードした12文字のシーケンスになることがある。
  Basic Multilingual Plane(BMP)にない文字は、コードポイント表現とサロゲート表現(High, Lowの組み合わせ)を持つ。
  🎼=U+1D11E -> "~uD834~uDD1E"

  JSONの文字列パーサーは、一度に一文字ではなく文字のペアをあつかう必要がある。
  Functor, Applicativeは一つの要素しか扱えないため、Monadを使う。

  Monadは、２番目の操作が最初の操作結果に依存できるように、コンテキスト内で操作を順番に並べることができる。

  入力から1文字だけでなく2文字を読んで、有効なサロゲートペアが見つかるか確認する。
  do構文は、モナド演算を連続させる >>=（モナド結合演算）のシンタックスシュガー。
  最初の文字を読み取って、それがサロゲートかに応じて異なる処理を選択する。
  これは、ParserのMonadインスタンスでないとできない。
-}
instance Monad (Parser i) where
  p >>= f = Parser $ \input -> case runParser p input of
    Nothing -> Nothing
    Just (rest, o) -> runParser (f o) rest

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString') -- 1. 「"」を解析して、残りの文字列をjString'に通す。その結果を最後にJStringでラップ。
  where
    jString' = do
      optFirst <- optional jsonChar -- 2. optional関数で、最初の文字をパースして取得。
      case optFirst of
        Nothing -> "" <$ char '"' -- 3. 最初の文字がない場合、入力は空なので末尾の「"」にマッチするようにして、空の文字列を出力として返す。
        Just first
          | not (isSurrogate first) -> -- 4. 最初の文字があり、かつサロゲートでない場合
              (first :) <$> jString' -- 5. jString'パーサーを残りの入力に再帰的に実行して、Stringパーサーみたいに、パースした文字と残りを返す
        Just first -> do
          -- 6. 4でない場合、つまり最初の文字がサロゲートである場合
          second <- jsonChar -- 7. 2番目の文字を解析して取得
          if isHighSurrogate first && isLowSurrogate second -- 8. 最初の文字がHighサロゲートで2番目がLowサロゲートの場合（有効なペアの場合）
            then (combineSurrogates first second :) <$> jString' -- 9. ペアを結合し、残りの文字列をjString'パーサーで再帰的に解析し、結合された文字を残りの出力とまとめて返す
            else empty -- 10. そうでなければ、無効なペアなので失敗する

{-
  For test / JString
  JSON文字列ジェネレータであるjStringGenを使って、任意のJSON文字列を生成する。

  forAllShrink関数は、入力生成と失敗時の入力縮小を自動で行う。
-}
prop_genParseJString :: Property
prop_genParseJString =
  forAllShrink jStringGen shrink $ \js ->
    case runParser jString (show js) of
      Nothing -> False
      Just (_, o) -> o == js

{-
  JNumber Parser
  JSONの数値はさまざまな形式にできる。
  フォーマットごとに別々のパーサーを書いて、それらを組み合わせて数値パーサーを実装する。
-}

{-
  整数パーサー
-}
-- 符号なし整数
-- digitsToNumberは数値のリストを整数に変換する
jUInt :: Parser String Integer
jUInt =
  (\d ds -> digitsToNumber 10 0 (d : ds))
    <$> digit19
    <*> digits -- 1桁目が1-9の場合に続く複数桁の数字をパース
      <|> fromIntegral
    <$> digit -- 単一の数字0-9に対応

-- 1-9のパース
-- satisfy でパースした文字に対して digitToInt を適用して、Char を整数値に変換
digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

-- 0-9 の数字の1回以上の繰り返しをパースしてリストとして返す
-- some は指定されたパーサーを1回以上実行
digits :: Parser String [Int]
digits = some digit

-- 符号サポート追加
jInt' :: Parser String Integer
jInt' = signInt <$> optional (char '-') <*> jUInt

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i
signInt _ i = i

{-
  小数部・指数部パーサー
-}
jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp =
  (char 'e' <|> char 'E')
    *> (signInt <$> optional (char '+' <|> char '-') <*> jUInt)

{-
  Number パーサー
  上のやつを組み合わせてつくる
-}
jInt :: Parser String JValue
jInt = JNumber <$> jInt' <*> pure [] <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp

jIntFrac :: Parser String JValue
jIntFrac = (\i f -> JNumber i f 0) <$> jInt' <*> jFrac

jIntFracExp :: Parser String JValue
jIntFracExp = (\ ~(JNumber i f _) e -> JNumber i f e) <$> jIntFrac <*> jExp

jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt

{-
  For test / JNumber
-}
prop_genParseJNumber :: Property
prop_genParseJNumber =
  forAllShrink jNumberGen shrink $ \jn ->
    case runParser jNumber (show jn) of
      Nothing -> False
      Just (_, o) -> o == jn

{-
  JArray Parser
  JSON配列はカンマで区切られた任意のJSON型で、項目の周囲に任意の数のJSON空白を含むこともできる。
  それを処理するために再帰的な構造になる。
-}
-- 空白ヘルパー
surroundedBy ::
  Parser String a -> Parser String b -> Parser String a
surroundedBy p1 p2 = p2 *> p1 <* p2

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s =
  (:)
    <$> v
    <*> many (s *> v)
      <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t') -- manyは0以上繰り返す

-- JArray
jArray :: Parser String JValue
jArray =
  JArray
    <$> ( char '['
            *> (jValue `separatedBy` char ',' `surroundedBy` spaces)
            <* char ']'
        )

{-
  For test / JArray

  sized jArrayGen ジェネレータで、任意のJSON配列を生成、文字列化、それをパースすると元の配列と等しくなる。
  sizedは、QuickCheckの生成サイズを操作できる。
  counterexampleは、QuickCheckエラーレポートに追加情報を加えるもの。
-}
prop_genParseJArray :: Property
prop_genParseJArray =
  forAllShrink (sized jArrayGen) shrink $ \ja -> do
    jas <- dropWhile isSpace <$> stringify ja
    return . counterexample (show jas) $ case runParser jArray jas of
      Nothing -> False
      Just (_, o) -> o == ja

{-
  JObject Parser
  JSON Objectパーサーは、これまで学んだことをそのまま書く。
-}
jObject :: Parser String JValue
jObject =
  JObject
    <$> (char '{' *> pair `separatedBy` char ',' `surroundedBy` spaces <* char '}')
  where
    pair =
      (\ ~(JString s) j -> (s, j))
        <$> (jString `surroundedBy` spaces)
        <* char ':'
        <*> jValue

{-
  For test / JObject

  JArrayとよく似てる
-}
prop_genParseJObject :: Property
prop_genParseJObject =
  forAllShrink (sized jObjectGen) shrink $ \jo -> do
    jos <- dropWhile isSpace <$> stringify jo
    return . counterexample (show jos) $ case runParser jObject jos of
      Nothing -> False
      Just (_, o) -> o == jo

{-
  JSON Parser
-}
jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' =
      jNull
        <|> jBool
        <|> jString
        <|> jNumber
        <|> jArray
        <|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
  Just ("", j) -> Just jArray
  _ -> Nothing

{-
  For test / JSON
-}
prop_genParseJSON :: Property
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json

{-
  Test
-}
runTests :: IO ()
runTests = do
  putStrLn "== prop_genParseJString =="
  quickCheck prop_genParseJString

  putStrLn "== prop_genParseJNumber =="
  quickCheck prop_genParseJNumber

  putStrLn "== prop_genParseJArray =="
  quickCheck prop_genParseJArray

  putStrLn "== prop_genParseJObject =="
  quickCheck prop_genParseJObject

  putStrLn "== prop_genParseJSON =="
  quickCheck prop_genParseJSON

{-
  For test / JSON Data Type / Show instance and method
-}
instance Show JValue where
  show value = case value of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JString s -> show s
    JNumber s [] 0 -> show s
    JNumber s f 0 -> show s ++ "e" ++ B.concatMap show f
    JNumber s [] e -> show s ++ "e" ++ show e
    JNumber s f e -> show s ++ "." ++ B.concatMap show f ++ "e" ++ show e
    JArray a -> "[" ++ B.intercalate ", " (map show a) ++ "]"
    JObject o -> "{" ++ B.intercalate ", " (map showKV o) ++ "}"
    where
      showKV (k, v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ B.concatMap showJSONString s ++ "\""

-- JSONの制御文字の定義がHaskellと異なるため、Data.Char.isControlは使わない
isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/' -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
  _ -> [c]
  where
    showJSONNonASCIIChar c =
      let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a

{-
  For test / Scala Generator
-}
jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary

{-
  For test / String Generator
-}
jsonStringGen :: Gen String
jsonStringGen =
  concat
    <*> listOf
      ( oneof
          [ vectorOf 1 arbitraryUnicodeChar,
            escapedUnicodeChar
          ]
      )
  where
    escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
    hexDigitLetters = ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen

{-
  For test / Composite Generator
-}
jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
  where
    objKV n = (,) <$> jsonStringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen =
  if n < 5
    then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
    else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
  where
    scalarGens = [jNullGen, jBoolGen, jNumberGen, jStringGen]
    compositeGens n = [jArrayGen n, jObjectGen n]

instance Arbitrary JValue where
  arbitrary = sized jValueGen
  shrink = genericShrink

{-
  For test / Adding Whitespace
  show 関数は純粋なので、任意の量の空白を生成できない。
  Gen モナドを使用する
-}
-- JValueの構造を走査して、その一部を再帰的に表示し、任意の空白で埋める
jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . fromIntegral)
    . listOf
    . elements
    $ [' ', '\n', '\r', '\t']

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . B.intercalate ","

    go value = case value of
      JArray element ->
        mapM (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _ -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"
