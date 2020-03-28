module Lang where

type Lang =
    { batsNearby :: String
    , pitNearby :: String
    , wumpusNearby :: String
    , roomsNearby :: String
    , arrows :: String
    , whatDirection :: String
    , unknownDirection :: String
    , moveTo :: String
    , missed :: String
    , bye :: String
    , unknownCommand :: String
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
    , unknownDirection: "Unknown direction"
    , moveTo: "Move to"
    , missed: "You missed and seemed to frighten off Wumpus"
    , bye: "Bye"
    , unknownCommand: "Unknown command"
    , cancelCommand: "Cancel command"
    , shoot: "Shoot"
    , move: "Move"
    , wumpusKill: "Wumpus killed you :("
    , fell: "You fell into a pit :("
    , outOfArrows: "Out of arrows :("
    , batsMoveYou: "Bats move you to another room"
    , win: "You win"
    , youAreHere: "You are in a room #"
    }

ru :: Lang
ru =
    { intro: "Победи Вумпуса. Ты в комнате"
    , batsNearby: "Летучие мыши рядом"
    , pitNearby: "Яма рядом"
    , wumpusNearby: "Вумпус рядом"
    , roomsNearby: "Комнаты рядом"
    , arrows: "Стрелы"
    , whatDirection: "Куда"
    , unknownDirection: "Незнамо куда"
    , cancelCommand: "Отменить команду"
    , moveTo: "Пошли на"
    , missed: "Промазали и, похоже, спугнули Вумпуса"
    , bye: "Покедова"
    , unknownCommand: "Не знаю такую команду"
    , shoot: "Стрелять"
    , move: "Идти"
    , wumpusKill: "Вумпус тебя съел :("
    , fell: "Упали в яму :("
    , outOfArrows: "Стрелы закончились :("
    , batsMoveYou: "Летучие мыши перенесли вас в другую комнату"
    , win: "Победа!"
    , youAreHere: "Вы в комнате №"
    }
