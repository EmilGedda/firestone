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

data EntityType = Minion 
                | Hero deriving (Ord, Show, Eq)

data MinionRace = Demon
                | Dragon
                deriving Show

data EventType = OnDamage 
               | OnTurnEnd deriving Show

data StatusEffects = Stealth 
                   | SpellDamage Integer 
                   | CantAttack deriving Show

data CardSet = Basic
             | Classic 
             | JourneyToUnGoro
             | KnightsOfTheFrozenThrone
             | KoboldsAndCatacombs
             | TheWitchwood
             deriving Show

data Rarity = None
            | Common
            | Rare
            | Epic
            | Legendary
            deriving Show

data Event = Event deriving Show

data Card = Card { cardName    :: Text
                 , manaCost    :: Integer
                 , health      :: Integer
                 , attack      :: Integer
                 , cardType    :: EntityType
                 , cardSet     :: CardSet
                 , heroType    :: Maybe HeroType
                 , rarity      :: Rarity
                 , race        :: Maybe MinionRace
                 , events      :: Map.Map EventType Event
                 , effects     :: [StatusEffects]
                 , description :: Text
                 } deriving Show

card :: Card
card = Card "" 0 0 0 Minion Basic Nothing None Nothing Map.empty [] ""

cardDefinitions :: [Card]
cardDefinitions = sortBy (comparing cardName)
                    [ card { cardName = "Dalaran Mage"
                           , manaCost = 3
                           , health   = 4
                           , attack   = 1
                           , effects  = [SpellDamage 1]
                           , description = "Spell Damage +1"
                           }
                    , card { cardName = "Defender"
                           , manaCost = 1
                           , health   = 1
                           , attack   = 2
                           , cardSet  = Classic
                           , rarity   = Common
                           , heroType = Just Paladin
                           }
                    , card { cardName = "Imp"
                           , manaCost = 1
                           , health   = 1
                           , attack   = 1
                           , cardSet  = Classic
                           , rarity   = Common
                           , race     = Just Demon
                           }
                    , card { cardName = "Ogre Magi"
                           , manaCost = 4
                           , health   = 4
                           , attack   = 4
                           , effects  = [SpellDamage 1]
                           , description = "Spell Damage +1"
                           }
                    , card { cardName = "War Golem"
                           , manaCost = 7
                           , health   = 7
                           , attack   = 7 
                           }
                    , card { cardName = "Ancient Watcher"
                           , manaCost = 2
                           , health   = 5
                           , attack   = 4
                           , cardSet  = Classic
                           , rarity   = Rare
                           , effects  = [CantAttack]
                           , description = "Can't attack."
                           }
                    ]
