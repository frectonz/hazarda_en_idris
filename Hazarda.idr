import System.Random
import Data.String

record Game where
  constructor MkGame
  answer : Nat
  tries : Nat

Show Game where
  show (MkGame answer tries) = "Game (answer = \{show answer}, tries = \{show tries})"

newGame : IO Game
newGame = do
  answer <- randomRIO (the Int32 0, the Int32 100)
  pure $ MkGame (cast answer) 0

data Guess = Quit | NotNumber | OutOfBounds | Number Nat

Show Guess where
  show Quit = "👋 Good Bye."
  show NotNumber = "😞 Guess is not a number."
  show OutOfBounds = "😞 Guess is out of bounds."
  show (Number x) = "🎲 You guessed \{show x}."

getGuess : IO Guess
getGuess = do
  putStr "🎯 Enter guess (q to Quit): "
  guess <- getLine
  pure (if guess == "q" then Quit else (checkGuess $ parsePositive guess))
  where
    checkGuess : Maybe Nat -> Guess
    checkGuess Nothing = NotNumber
    checkGuess (Just x) = if (withInBounds x) then OutOfBounds else Number x
    where
      withInBounds : Nat -> Bool
      withInBounds x = x < 0 || x > 100

incrementTry : Game -> Game
incrementTry game =
  { tries $= (+ 1) } game

data CheckResult = GreaterThan | LessThan | Equal

Show CheckResult where
  show GreaterThan = "👆 Try a bigger number."
  show LessThan = "👇 Try a smaller number."
  show Equal = "🎉 You got it."

checkGuess : Nat -> Game -> CheckResult
checkGuess guess game =
  if game.answer > guess then
    GreaterThan
  else if game.answer < guess then
    LessThan
  else
    Equal

gameLoop : Game -> IO ()
gameLoop game = do
  guess <- getGuess
  putStrLn (show guess)

  Number guess <- pure guess
    | Quit => putStrLn "The answer was \{show game.answer}."
    | _ => gameLoop game

  result <- pure $ checkGuess guess game
  putStrLn (show result)

  Equal <- pure result
    | _ => gameLoop $ incrementTry game
  putStrLn "It took you \{show (game.tries + 1)} tries."

main : IO ()
main = do
  putStrLn "Guess a number between 0 and 100."
  game <- newGame
  gameLoop game

