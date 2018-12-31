module Firestone.Construct where

import qualified Data.Map.Strict as Map
import Data.Text

newtype ID = ID Text deriving (Show, Ord, Eq)

data Entity = Entity { entityId :: ID
                     , entityName :: Text
                     , entityDamageTaken :: Integer
                     , entityOwnerId :: ID
                     } deriving Show

data Card = Card { cardName :: Text
                 , cardId :: ID
                 , cardManaCost :: Integer
                 , playable :: Bool
                 } deriving Show

data HeroPower = HeroPower { heroPowerName :: Text
                           , available :: Bool
                           , manaCost :: Integer
                           } deriving Show

data Hero = Hero { heroId :: ID
                 , heroName :: Text
                 , heroDamageTaken :: Integer
                 , fatigue :: Integer
                 , heroOwnerId :: ID
                 , heroPower :: HeroPower
                 } deriving Show

-- TODO
data Quest = Quest deriving Show
data Secret = Secret deriving Show

data Player = Player { playerId :: ID
                     , deck :: [Card]
                     , hand :: [Card]
                     , board :: [Entity]
                     , mana :: Integer
                     , manaCrystals :: Integer
                     , activeQuest :: Maybe Quest
                     , activeSecrets :: [Secret]
                     , hero :: Hero
                     } deriving Show

data Game = Game { playerIdInTurn :: ID
                 , players :: Map.Map ID Player
                 , minionIdsSummoned :: [ID]
                 , counter :: Integer
                 , seed :: Integer
                 } deriving Show

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
createPlayer id hero = Player id [] [] [] 1 1 Nothing [] hero

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
                  
             

