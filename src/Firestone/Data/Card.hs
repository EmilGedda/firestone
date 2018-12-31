module Firestone.Data.Card where

import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text

data HeroType = Paladin 
               | Rogue 
               | Shaman 
               | Mage 
               | Hunter 
               deriving Show

data EntityType = Minion | Hero deriving (Ord, Show, Eq)

data EventType = OnDamage | OnTurnEnd deriving Show

data StatusEffects = StatusEffects deriving Show

data CardSet = Basic deriving Show

data Rarity = None
            | Common
            deriving Show

data Event = Event deriving Show

data Card = Card { cardName :: Text
                 , manaCost :: Integer
                 , health :: Integer
                 , attack :: Integer
                 , spellDamage :: Maybe Integer
                 , cardType :: EntityType
                 , cardSet :: CardSet
                 , heroType :: Maybe HeroType
                 , rarity :: Rarity
                 , events :: Map.Map EventType Event
                 , effects :: [StatusEffects]
                 , description :: Text
                 } deriving Show

card :: Card
card = Card "" 0 0 0 Nothing Minion Basic Nothing None Map.empty [] ""

cardDefinitions :: [Card]
cardDefinitions = sortBy (comparing cardName)
                    [ card { cardName = "Dalaran Mage"
                           , manaCost = 3
                           , health   = 4
                           , attack   = 1
                           , spellDamage = Just 1
                           , description = "Spell Damage +1"
                           }
                    , card { cardName = "Defender"
                           , manaCost = 1
                           , health   = 1
                           , attack   = 2
                           , rarity   = Common
                           , heroType = Just Paladin
                           }
                    , card { cardName = "Imp"
                           , manaCost = 1
                           , health   = 1
                           , attack   = 1
                           }
                    , card { cardName = "Ogre Magi"
                           , manaCost = 4
                           , health   = 4
                           , attack   = 4
                           , spellDamage = Just 1
                           , description = "Spell Damage +1"
                           }
                    , card { cardName = "War Golem"
                           , manaCost = 7
                           , health   = 7
                           , attack   = 7 
                           }
                    ]
