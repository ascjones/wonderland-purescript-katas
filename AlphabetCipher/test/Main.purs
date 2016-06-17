module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit.Assert as Assert
import Test.Unit (test, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import AlphabetCipher (encode, decode, decipher)

main :: forall eff. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | eff) Unit
main = runTest do
  suite "encode" do
    test "can encode given a secret keyword" do
      Assert.equal "hmkbxebpxpmyllyrxiiqtoltfgzzv" $ encode "vigilance" "meetmeontuesdayeveningatseven"
      Assert.equal "egsgqwtahuiljgs" $ encode "scones" "meetmebythetree"
  suite "decode" do
    test "can decode an encyrpted message given a secret keyword" do
      Assert.equal "meetmeontuesdayeveningatseven" $ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv"
      Assert.equal "meetmebythetree" $ decode "scones" "egsgqwtahuiljgs"
  suite "decipher" do
    test "can extract the secret keywork given an encrypted message and the original message" do
      Assert.equal "vigilance" $ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog"
      Assert.equal "scones" $ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs"
