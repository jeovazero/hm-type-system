{-# LANGUAGE OverloadedStrings #-}

module HM.NameSeed (
  NameSeed (..),
  startNameSeed,
  increaseNsPrefix,
  splitNsPrefix,
  splitNsNumber,
  varNameSequence,
  varNameNs,
  increaseNsIndex,
  prefixSequence,
  zipWithNs,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.Word (Word8)

data NameSeed = NameSeed Int Int

instance Show NameSeed where
  show ns = B8.unpack $ buildNsName "" ns

charCodeA = fromIntegral $ ord 'a'

startNameSeed = NameSeed 1 1

increaseNsPrefix (NameSeed c _) = NameSeed (c + 1) 1

increaseNsIndex (NameSeed c i) = NameSeed c (i + 1)

zipWithNs xs ns = zipWithNs' xs (ns, [])

zipWithNs' [] (ns, acc) = (ns, reverse acc)
zipWithNs' (x : xs) (ns, acc) = zipWithNs' xs (ns', (ns, x) : acc)
 where
  ns' = increaseNsPrefix ns

varNameSequence ns = seqNS' ns 0
 where
  seqNS' ns i = B8.concat [buildNsName "" ns, "'", B8.pack $ show i] : seqNS' ns (i + 1)

splitNsNumber (NameSeed c i) = (NameSeed c (i * 2), NameSeed c (i * 2 + 1))

splitNsPrefix (NameSeed c i) = (NameSeed (c * 2) i, NameSeed (c * 2 + 1) i)

buildNsName separator ns@(NameSeed c i) = B8.concat [nsPrefix ns, separator, B8.pack $ show i]

varNameNs ns = buildNsName "" ns

nsPrefix (NameSeed c _) = B.pack $ base26 c []

prefixSequence ns = nsPrefix ns : prefixSequence (increaseNsPrefix ns)

base26 :: Int -> [Word8] -> [Word8]
base26 n acc
  | n == 0 = charCodeA : acc
  | otherwise = next
 where
  q = div n 26
  r = fromIntegral $ mod n 26
  acc' = (r + charCodeA : acc)
  next = if q > 0 then base26 (q - 1) acc' else acc'
