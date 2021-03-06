module Lang where

type Lang =
    { batsNearby :: String
    , pitNearby :: String
    , wumpusNearby :: String
    , roomsNearby :: String
    , arrows :: String
    , whatDirection :: String
    , moveTo :: String
    , missed :: String
    , cancelCommand :: String
    , shoot:: String
    , move :: String
    , wumpusKill :: String
    , fell :: String
    , outOfArrows :: String
    , batsMoveYou :: String
    , intro :: String
    , win :: String
    , youAreHere :: String
    , youSeeRooms :: String
    }

en :: Lang
en =
    { intro: "Hunt the Wumpus. You are in a room"
    , batsNearby: "Bats nearby"
    , pitNearby: "Pit nearby"
    , wumpusNearby: "Wumpus nearby"
    , roomsNearby: "Rooms nearby"
    , arrows: "Arrows"
    , whatDirection: "What direction"
    , moveTo: "Move to"
    , missed: "You missed and seemed to frighten off Wumpus"
    , cancelCommand: "Cancel command"
    , shoot: "Shoot"
    , move: "Move"
    , wumpusKill: "Wumpus killed you :("
    , fell: "You fell into a pit :("
    , outOfArrows: "Out of arrows :("
    , batsMoveYou: "Bats move you to another room"
    , win: "You win"
    , youAreHere: "You are in a room #"
    , youSeeRooms: "You see rooms ##"
    }

ru :: Lang
ru =
    { intro: "Победи Вумпуса. Ты в комнате"
    , batsNearby: "В соседней комнате летучие мыши"
    , pitNearby: "В соседней комнате яма"
    , wumpusNearby: "В соседней комнате Вумпус"
    , roomsNearby: "Комнаты рядом"
    , arrows: "Стрелы"
    , whatDirection: "Куда"
    , cancelCommand: "Отменить команду"
    , moveTo: "Пошли на"
    , missed: "Промазали и, похоже, спугнули Вумпуса"
    , shoot: "Стрелять"
    , move: "Идти"
    , wumpusKill: "Вумпус тебя съел :("
    , fell: "Упали в яму :("
    , outOfArrows: "Стрелы закончились :("
    , batsMoveYou: "Летучие мыши перенесли вас в другую комнату"
    , win: "Победа!"
    , youAreHere: "Вы в комнате №"
    , youSeeRooms: "Вы видите команты №№"
    }
