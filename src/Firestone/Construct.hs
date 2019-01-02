module Firestone.Construct where

import Prelude hiding (id)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Lens
import Data.Text hiding (dropWhile, drop, head, length)

newtype ID = ID Text deriving (Show, Ord, Eq)

data Entity = Entity { _entityId :: ID
                     , _entityName :: Text
                     , _entityDamageTaken :: Integer
                     , _entityOwnerId :: ID
                     } deriving Show
makeLenses ''Entity

data Card = Card { _cardName :: Text
                 , _cardId :: ID
                 , _cardManaCost :: Integer
                 , _playable :: Bool
                 } deriving Show
makeLenses ''Card

data HeroPower = HeroPower { _heroPowerName :: Text
                           , _available :: Bool
                           , _manaCost :: Integer
                           } deriving Show
makeLenses ''HeroPower

data Hero = Hero { _heroId :: ID
                 , _heroName :: Text
                 , _heroDamageTaken :: Integer
                 , _fatigue :: Integer
                 , _heroOwnerId :: ID
                 , _heroPower :: HeroPower
                 } deriving Show
makeLenses ''Hero

-- TODO
data Quest = Quest deriving Show
data Secret = Secret deriving Show

data Player = Player { _playerId :: ID
                     , _deck :: [Card]
                     , _hand :: [Card]
                     , _board :: [Entity]
                     , _mana :: Integer
                     , _manaCrystals :: Integer
                     , _activeQuest :: Maybe Quest
                     , _activeSecrets :: [Secret]
                     , _hero :: Hero
                     } deriving Show

makeLenses ''Player

data Game = Game { _currentPlayer :: ID
                 , _players :: Map.Map ID Player
                 , _minionIdsSummoned :: [ID]
                 , _counter :: Integer
                 , _seed :: Integer
                 } deriving Show

makeLenses ''Game

class Character a where
    id :: a -> ID
    name :: a -> Text
    attack :: a -> Integer
    health :: a -> Integer

createHeroPower :: Text -> HeroPower
createHeroPower text = HeroPower text True 2

createHero :: ID -> Text -> ID -> HeroPower -> Hero
createHero id name ownerId heroPower = Hero id name 0 0 ownerId heroPower

createPlayer :: ID -> Hero -> Player
createPlayer id hero = Player id [] [] [(Entity (ID "m1") "Imp" 0 id)] 1 1 Nothing [] hero

emptyState :: Game
emptyState = Game p1
                  (Map.fromList [ (p1, createPlayer p1 h1)
                                , (p2, createPlayer p2 h2)
                                ])
                  []
                  3
                  0
            where p1 = ID "p1"
                  p2 = ID "p2"
                  h1 = createHero (ID "h1") "Jaina Proudmoore" p1 $ createHeroPower "Fireblast"
                  h2 = createHero (ID "h2") "Jaina Proudmoore" p2 $ createHeroPower "Fireblast"

getPlayerIDs :: Game -> [ID]
getPlayerIDs state = Map.keys $ state ^. players

-- TODO: no fromJust
getPlayer :: Game -> ID -> Player
getPlayer state id = fromJust $ state ^. players ^.at id

getPlayerCollection :: Getting a Player a -> Game -> ID -> a
getPlayerCollection collection state id = getPlayer state id ^. collection

getHand :: Game -> ID -> [Card]
getHand = getPlayerCollection hand

getDeck :: Game -> ID -> [Card]
getDeck = getPlayerCollection deck

getMinions :: Game -> ID -> [Entity]
getMinions = getPlayerCollection board

getAllMinions :: Game -> [Entity]
getAllMinions = view (players . traverse . board) 

nextPlayer :: Game -> ID
nextPlayer state = head . drop 1 . dropWhile (/= (state ^. currentPlayer)) 
                   . cycle $ getPlayerIDs state

otherPlayer :: Game -> ID -> ID
otherPlayer state id = nextPlayer $ set currentPlayer id state

boardFull :: Game -> ID -> Bool
boardFull state = (>= 7) . length . getMinions state

getSpellDamage state id = getMinions state id 
