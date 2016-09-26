data Player = Player  
	{ pHealth   :: Int
	, pAttack   :: Int
	, pDefense  :: Int
	, pWeapon   :: Equipment
	, pArmor    :: Equipment
	} deriving Show
data Monster = Monster 
	{ mHealth   :: Int
	, mAttack   :: Int
	, mDefense  :: Int
	, mDrop      :: [Equipment] 
	} deriving Show
data Equipment = 
	Weapon  { eAttack  :: Int } | 
	Armor   { eDefense :: Int } | 
	Poison  { eHealth  :: Int } deriving Show
data Result = Win Player | Lose deriving Show

getResultPlayer :: Result -> Player
getResultPlayer (Win player) = player

checkItWasFinalFight :: Result -> Bool
checkItWasFinalFight Lose = True
checkItWasFinalFight    _ = False

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle player     [] = Win player
gloriousBattle player (m:ms) = if checkItWasFinalFight miniResult
	then Lose
	else gloriousBattle (getResultPlayer miniResult) ms where 
		miniResult = fight player m True

fight :: Player -> Monster -> Bool -> Result
fight player monster True = if mNewMHealth == 0 
	then Win (pickUpDrop player (mDrop monster))
	else fight player newMonster False where 
		mNewMHealth = max 0 ((mHealth monster) - (max 1 ((pAttack player) + (eAttack (pWeapon player)) - (mDefense monster))));
		newMonster  = monster { mHealth = mNewMHealth }		
fight player monster False = if (pNewHealth == 0) 
	then Lose
	else fight newPlayer monster True where 
		pNewHealth = max 0 ((pHealth player) - (max 1 ((mAttack monster) - (pDefense player) - (eDefense (pArmor player)))))
		newPlayer  = player { pHealth = pNewHealth }
		
pickUpDrop :: Player -> [Equipment] -> Player
pickUpDrop player [] = player
pickUpDrop player (e:es) = pickUpDrop (pickUpOne player e) es

pickUpOne :: Player -> Equipment -> Player
pickUpOne player Weapon { eAttack = a } = if (eAttack (pWeapon player)) < a
	then player { pWeapon = Weapon { eAttack = a } }
	else player
pickUpOne player Armor { eDefense = d } = if (eDefense (pArmor player)) < d
	then player { pArmor = Armor { eDefense = d } }
	else player
pickUpOne player Poison { eHealth = h } = player { pHealth = (pHealth player) + h }