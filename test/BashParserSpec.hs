{-# LANGUAGE OverloadedStrings #-}


module BashParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit
import Data.Attoparsec.Text as P

import BashParser


spec :: Spec
spec = do
  describe "name parser" $
    fromHUnitTest nameParserTest

  describe "metacharacter parser" $
    fromHUnitTest metacharacterParserTest

  describe "unquotedChar parser" $
    fromHUnitTest unquotedCharParserTest
    

nameParserTest :: Test
nameParserTest = TestList
  [ "頭が小英字" ~: P.parseOnly name "t1esT" ~?= Right "t1esT"
  , "頭がアンダーライン" ~: P.parseOnly name "_Text0" ~?= Right "_Text0"
  , "頭が大英字" ~: P.parseOnly name "T0ext" ~?= Right "T0ext"
  , "頭が数字" ~: P.parseOnly name "4tEst_" ~?= Left "Failed reading: satisfy"
  , "頭が記号" ~: P.parseOnly name "@ErrorCase" ~?= Left "Failed reading: satisfy"
  ]

metacharacterParserTest :: Test
metacharacterParserTest = TestList
  [ "パイプ" ~: P.parseOnly metacharacter "|test" ~?= Right '|'
  , "アンパサンド" ~: P.parseOnly metacharacter "&test" ~?= Right '&'
  , "セミコロン" ~: P.parseOnly metacharacter ";test" ~?= Right ';'
  , "開き小かっこ" ~: P.parseOnly metacharacter "(test" ~?= Right '('
  , "閉じ小かっこ" ~: P.parseOnly metacharacter ")test" ~?= Right ')'
  , "小なり" ~: P.parseOnly metacharacter "<test" ~?= Right '<'
  , "大なり" ~: P.parseOnly metacharacter ">test" ~?= Right '>'
  , "スペース" ~: P.parseOnly metacharacter " test" ~?= Right ' '
  , "水平タブ" ~: P.parseOnly metacharacter "\ttest" ~?= Right '\t'
  , "メタキャラクタでない文字" ~: P.parseOnly metacharacter "test" ~?= Left "Failed reading: satisfy"
  ]



unquotedCharParserTest :: Test
unquotedCharParserTest = TestList
  [ "エスケープされていない文字" ~: P.parseOnly (unquotedChar letter) "test" ~?= Right 't'
  , "エスケープされた文字" ~: P.parseOnly (unquotedChar letter) "\test" ~?= Right 't'
  ]
