module Firestone.Data.Card where

import qualified Data.Map.Strict as Map
import Control.Lens
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

data Card = Card { _cardName    :: Text
                 , _manaCost    :: Integer
                 , _health      :: Integer
                 , _attack      :: Integer
                 , _cardType    :: EntityType
                 , _cardSet     :: CardSet
                 , _heroType    :: Maybe HeroType
                 , _rarity      :: Rarity
                 , _race        :: Maybe MinionRace
                 , _events      :: Map.Map EventType Event
                 , _effects     :: [StatusEffects]
                 , _description :: Text
                 } deriving Show

makeLenses ''Card

card :: Card
card = Card "" 0 0 0 Minion Basic Nothing None Nothing Map.empty [] ""

cardDefinitions :: [Card]
cardDefinitions = sortBy (comparing _cardName)
                    [ card { _cardName = "Dalaran Mage"
                           , _manaCost = 3
                           , _health   = 4
                           , _attack   = 1
                           , _effects  = [SpellDamage 1]
                           , _description = "Spell Damage +1"
                           }
                    , card { _cardName = "Defender"
                           , _manaCost = 1
                           , _health   = 1
                           , _attack   = 2
                           , _cardSet  = Classic
                           , _rarity   = Common
                           , _heroType = Just Paladin
                           }
                    , card { _cardName = "Imp"
                           , _manaCost = 1
                           , _health   = 1
                           , _attack   = 1
                           , _cardSet  = Classic
                           , _rarity   = Common
                           , _race     = Just Demon
                           }
                    , card { _cardName = "Ogre Magi"
                           , _manaCost = 4
                           , _health   = 4
                           , _attack   = 4
                           , _effects  = [SpellDamage 1]
                           , _description = "Spell Damage +1"
                           }
                    , card { _cardName = "War Golem"
                           , _manaCost = 7
                           , _health   = 7
                           , _attack   = 7 
                           }
                    , card { _cardName = "Ancient Watcher"
                           , _manaCost = 2
                           , _health   = 5
                           , _attack   = 4
                           , _cardSet  = Classic
                           , _rarity   = Rare
                           , _effects  = [CantAttack]
                           , _description = "Can't attack."
                           }
                    ]
