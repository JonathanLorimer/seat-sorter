{-# LANGUAGE ScopedTypeVariables #-}
module SeatSorter where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

data Ticket = Ticket { event     :: String
                     , section   :: String
                     , row       :: String
                     , startseat :: Integer
                     , endseat   :: Integer } deriving (Eq, Ord, Show)

type EventSectionRow = String

newtype TicketError = TicketError Text deriving (Eq, Ord, Show)
type Parser = Parsec TicketError Text

groupTickets :: [Ticket] -> Map EventSectionRow (NonEmpty Ticket)
groupTickets = foldr insertTicket M.empty
  where
    prepend (new :| _) old = new <| old
    insertTicket t@(Ticket e s r _ _) =
      M.insertWith prepend (e ++ s ++ r) (t :| [])

ticketParser :: Parser Ticket
ticketParser = do
  e  <- manyTill alphaNumChar $ char ','
  s  <- manyTill alphaNumChar $ char ','
  r  <- manyTill alphaNumChar $ char ','
  ss <- manyTill digitChar $ char ','
  es <- many digitChar
  _  <- eof
  return $ Ticket e s r (read ss) (read es)

parseTicket :: String -> Maybe Ticket
parseTicket = parseMaybe ticketParser . T.pack

sequences :: NonEmpty Ticket -> [[Ticket]]
sequences = groupSequential . L.sortOn startseat . NE.toList

groupSequential :: [Ticket] -> [[Ticket]]
groupSequential = foldr go [[]]
  where
    go t' [[]] = [[t']]
    go t' ((t:ts):xs) = if isSequential t t'
                        then (t':t:ts):xs
                        else [t']:(t:ts):xs
    isSequential t1 t2 =  endseat t1 + 1 == startseat t2
                          || startseat t1 == (endseat t2 + 1)


contPerms :: forall a . [a] -> [[[a]]]
contPerms [] = []
contPerms [x] = [[[x]]]
contPerms x = concatMap (\(y:z:_) -> [y,z] : ((y:) <$> (contPerms z))) selections
  where
    trim [] = []
    trim [y] = [y]
    trim ys = L.tail . L.init $ ys
    selections :: [[[a]]]
    selections = zipWith (\i j -> [i, j]) (trim $ L.inits x) (trim $ L.tails x)

permuteConsecutive :: [[Ticket]] -> [[[Ticket]]]
permuteConsecutive xs = map (map (map foldTickets)) permutations
  where
    permutations = fmap (L.nub . contPerms) xs
    foldTickets [y] = y
    foldTickets ys  = L.foldl1' unsafeJoinTicket ys


joinNonConsecutive :: [[[Ticket]]] -> [[Ticket]]
joinNonConsecutive = foldr go [[]]
  where
    go :: [[Ticket]] -> [[Ticket]] -> [[Ticket]]
    go ts z = (++) <$> ts <*> z

joinTicket :: Ticket -> Ticket -> Maybe Ticket
joinTicket (Ticket e s r ss se) (Ticket e' s' r' ss' se')
  | e /= e' || s /= s' || r /= r' = Nothing
  | ss  == (se' - 1)              = Just $ Ticket e s r ss' se
  | ss' == (se - 1)               = Just $ Ticket e s r ss se'
  | otherwise                     = Nothing


unsafeJoinTicket :: Ticket -> Ticket -> Ticket
unsafeJoinTicket (Ticket e s r ss se) (Ticket _ _ _ ss' se')
  | (ss - 1)  == se' = Ticket e s r ss' se
  | (ss' - 1) == se  = Ticket e s r ss  se'


sample = [ "supersonic,B,1,3,4"
         , "supersonic,E,1,13,13"
         , "supersonic,B,1,5,6"
         , "supersonic,C,2,1,4"
         , "supersonic,C,3,1,1"
         , "supersonic,D,2,7,9"
         , "supersonic,D,2,10,10"
         ]

sample2 = [ "supersonic,B,1,1,1"
          , "supersonic,B,1,2,2"
          , "supersonic,B,1,3,3"
          , "supersonic,B,1,4,4"
          , "supersonic,B,1,7,7"
          ]

n :: Eq a => [a] -> [a]
n = L.nub
f1 = L.foldl1'
cm = catMaybes

parsed = cm $ parseMaybe ticketParser . T.pack <$> sample2
seqs = sequences . NE.fromList $ parsed
p = joinNonConsecutive $ permuteConsecutive seqs

--
-- $> p
--
-- $> n $ (contPerms [1,2,3,4] :: [[[Integer]]])



