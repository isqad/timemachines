module Shop where

data Client = GovOrg String Address
            | Company String Integer String String Person Address
            | Individual Person String Bool Address
            | OnlineStore String String Person Address
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

data GenderStatistic = GenderStatistic Integer Integer Integer
  deriving Show

address = Address (Country "Russia") (City "Moscow") (Street "Lenina") 13
client1 = GovOrg "NT" address
client2 = Individual (Person "Andrew" "Shalaev" Male) "Foo bar" False address
client3 = Company "Cows" 1 "foo" "bar" (Person "Lisa" "Hacker" Female) address
client4 = Company "Cows2" 1 "foo" "bar" (Person "Lisa" "Hacker" Female) address

clientGendersStatistic :: [Client] -> GenderStatistic
clientGendersStatistic clients = genderStatistic (map clientGender clients) (GenderStatistic 0 0 0)
  where genderStatistic [] stat = stat
        genderStatistic (x:xs) stat
          = genderStatistic xs (genderStatIncr x stat)
            where genderStatIncr (Female) (GenderStatistic males females unknowns) = GenderStatistic males (females + 1) unknowns
                  genderStatIncr (Male) (GenderStatistic males females unknowns) = GenderStatistic (males + 1) females unknowns
                  genderStatIncr (Unknown) (GenderStatistic males females unknowns) = GenderStatistic males females (unknowns + 1)

        clientGender (Individual (Person _ _ gender) _ _ _) = gender
        clientGender (OnlineStore _ _ (Person _ _ gender) _) = gender
        clientGender (Company _ _ _ _ (Person _ _ gender) _) = gender
        clientGender _ = Unknown
