module Shop where

data Client = GovOrg String Address
            | Company String Integer String String Address
            | Individual Person String Bool Address
            | OnlineStore String String Address
            deriving Show

data Address = Address Country City Street Integer
  deriving Show

data Country = Country String
  deriving Show

data City = City String
  deriving Show

data Street = Street String
  deriving Show

data Person = Person String String Gender
  deriving Show

data Gender = Male | Female | Unknown
  deriving Show

data TimeMachine = FutureJourney Manufacturer Integer Price
                 | PastJourney Manufacturer Integer Price
                 deriving Show

data Manufacturer = Manufacturer Client
  deriving Show

data Price = Price Double Currency
  deriving Show

data Currency = Currency String
  deriving Show

