import Text.ParserCombinators.ReadP
import System.Environment (getArgs)
import Control.Applicative ((<|>))
import Data.List (intercalate, sort, find)
import Data.Time
import Control.Monad (when, unless)
-- import System.Directory (openFile)
import System.IO
import System.Exit (die, exitSuccess)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Foldable (forM_)


-- DATA TYPES --
data Task = Task { tid :: Int
                 , subject :: String
                 , projects :: [String]
                 , contexts :: [String]
                 , due :: Maybe Day
                 , completed :: TaskStatus
                 , archived :: Bool
                 , priority :: Priority
                 } deriving Eq

data TaskStatus = Complete | Incomplete deriving Eq
data Priority = Normal | Urgent deriving Eq

instance Show TaskStatus where
    show Incomplete = "[ ]"
    show Complete   = "[x]"

instance Show Priority where
    show Normal = ""
    show Urgent = "!!"

type TaskList = [Task]

instance Show Task where
  show (Task tid subject projects contexts due completed archived priority) =
       "id:" ++ (show tid) ++ ","
    ++ "subject:" ++ "\"" ++ subject ++ "\"" ++ ","
    ++ "projects:" ++ (show projects) ++ ","
    ++ "contexts:" ++ (show contexts) ++ ","
    ++ "due:" ++ (showMaybe due) ++ ","
    ++ "isComplete:" ++ (if completed == Complete then "True" else "False") ++ ","
    ++ "isArchived:" ++ (show archived) ++ ","
    ++ "isPriority:" ++ (if priority == Urgent then "True" else "False")

showMaybe :: Show a => Maybe a -> String
showMaybe maybeObj = case maybeObj of
                     Nothing   -> ""
                     Just obj  -> show obj

-- formats the date to look nicer
prettyDueString :: Maybe Day -> Day -> String
prettyDueString Nothing today = ""
prettyDueString (Just day) today
    | day == today             = "today"
    | day == (addDays 1 today) = "tomorrow"
    | (addDays 1 day) == today = "yesterday"
    | otherwise                = formatTime defaultTimeLocale "%a %b %d" day

formatTask :: Day -> Task -> String
formatTask today (Task tid subject _ _ due completed _ pri) = 
           (show tid) ++ "\t" 
        ++ (show completed) ++ "\t" 
        ++ (prettyDueString due today) ++ "\t"
        ++ subject 
        ++ (if pri == Urgent then ("\t" ++ show pri) else "")

-- wordsWhen copied from https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- splits the String whenever the predicate returns true.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
startsWith :: Char -> String -> Bool
startsWith _ "" = False
startsWith c s  = (head s) == c

splitBy :: Char -> String -> [String]
splitBy c s = wordsWhen (== c) s

splitToWords :: String -> [String]
splitToWords = splitBy ' '

--gets the lowest unused index >= 1.
--for example, if the used tids are [1,4,3] it returns 2.
--WOW IT'S POINTFREE!!!! (this comment was written at 2:30 AM)
findUnusedTaskIndex :: TaskList -> Int
findUnusedTaskIndex = smallestUnused . sort . map tid

--takes a sorted finite list of positive Int, returns first available index >= 1
smallestUnused :: [Int] -> Int
smallestUnused [] = 1
smallestUnused indices = let len = length indices
                         in if (len == last indices) then len + 1
                                                     else smallestUnused $ init indices


getContextsFromSubject :: String -> [String]
getContextsFromSubject = map tail . filter (startsWith '+') . splitToWords

getProjectsFromSubject :: String -> [String]
getProjectsFromSubject = map tail . filter (startsWith '@') . splitToWords

contains :: Eq a => a -> [a] -> Bool
contains a lst = case (find (== a) lst) of
                          Nothing -> False
                          Just _  -> True

-- PARSER COMBINATORS --

addCommand :: ReadP (Task, String)
addCommand = do
           string "add"
           skipSpaces
           (subject, dueDateString) <- parseAddedTaskSubject
           
           let projects = getProjectsFromSubject subject
           let contexts = getContextsFromSubject subject
           let newTask = Task 0 subject projects contexts Nothing Incomplete False Normal 
           
           -- I have decided not to allow subjects to contain quotes.
           -- otherwise I have to change my serialization syntax, 
           -- or implement lookahead in the parser.
           if (contains quoteChar subject) then pfail
           else return (newTask, dueDateString)

mutatingCommand :: ReadP Int
mutatingCommand = do
            string "complete" <|> string "delete" <|> string "archive" <|> string "unarchive" <|> string "urgent"
            skipSpaces
            taskNumStr <- munch1 isNumber
            let taskNum = read taskNumStr :: Int
            return taskNum


parseAddedTaskSubject :: ReadP (String, String)
parseAddedTaskSubject = do
                 (subject, dueDateString) <- subjectWithDue <++ subjectWithoutDue
                 return (subject, dueDateString)

subjectWithDue :: ReadP (String, String)
subjectWithDue = do
            subject <- many1 anyChar
            string " due "
            dueDateString <- munch1 always
            return (subject, dueDateString)

subjectWithoutDue :: ReadP (String, String)
subjectWithoutDue = do
                 subject <- munch1 always
                 let dueDateString = ""
                 return (subject, dueDateString)

setIdAndDue :: Task -> Int -> Maybe Day -> Task
setIdAndDue (Task _ subj proj cont _ comp isArc pri) newId newDue = 
  Task newId subj proj cont newDue comp isArc pri

setComplete :: Task -> Task
setComplete (Task tid subj proj cont due _ isArc pri) = 
  Task tid subj proj cont due Complete isArc pri

setUrgent :: Task -> Task
setUrgent (Task tid subj proj cont due comp isArc _) = 
  Task tid subj proj cont due comp isArc Urgent

setUnarchived :: Task -> Task
setUnarchived (Task tid subj proj cont due comp _ pri) = 
  Task tid subj proj cont due comp False pri

setArchived :: Task -> Task
setArchived (Task tid subj proj cont due comp _ pri) = 
  Task tid subj proj cont due comp True pri

taskFromAddCommand :: String -> Int -> Day -> Maybe Task
taskFromAddCommand cmd newId today = let parseResult = parseMaybe addCommand cmd
                                     in case parseResult of
                                        Nothing -> Nothing
                                        Just (t, dueStr) -> Just (setIdAndDue t newId (dayFromString dueStr today))

-- attempt to parse a String into a Day,
-- either of the form YYYY-MM-DD or a literal like "tomorrow"
dayFromString :: String -> Day -> Maybe Day
dayFromString dayStr today = case (parseMaybe dayString dayStr) of
                                  Just day -> Just day
                                  Nothing -> case (lookup dayStr langToDayDiff) of
                                                  Nothing -> Nothing
                                                  Just diff -> Just (addDays diff today) 

always :: Char -> Bool
always = const True

                

anyChar :: ReadP Char
anyChar = satisfy always

-- Just because I hate looking at escape sequences...
quoteChar :: Char
quoteChar = '\"'

isComma :: Char -> Bool
isComma c = (c == ',')

comma :: ReadP Char
comma = satisfy isComma

isQuotes :: Char -> Bool
isQuotes c = (c == quoteChar)

quotes :: ReadP Char
quotes = satisfy isQuotes

openingBracket :: ReadP Char
openingBracket = satisfy (\c -> c == '[')

closingBracket :: ReadP Char
closingBracket = satisfy (\c -> c == ']')



-- map between natural language terms like "tomorrow" to the corresponding
-- number of days difference.
-- cases are of course non-exhaustive, this is just a proof of concept
langToDayDiff :: [(String, Integer)]
langToDayDiff = [ ("today", 0), ("tod", 0), 
                      ("tomorrow", 1), ("tom", 1),
                      ("next week", 7), ("yesterday", -1),
                      ("next month", 30)
                ] 
-- map between command line args and corresponding functions
-- the functions all mutate the TaskList in some way
-- and some may need to know today's date.
commandToFunc :: [(String, (TaskList -> String -> Day -> TaskList))]
commandToFunc = [ ("add", addTask), ("complete", completeTask), 
                  ("archive", setArchiveTask),
                  ("urgent", setUrgentTask), ("delete", deleteTask), ("unarchive", setUnarchiveTask)
                ]
legalCommands :: [String]
legalCommands = ["add", "complete", "archive", "urgent", "delete", "unarchive"]

parseProjects :: ReadP [String]
parseProjects = do 
              string "projects:"
              projects <- parseListOfStrings
              return projects

parseContexts :: ReadP [String]
parseContexts = do 
              string "contexts:"
              contexts <- parseListOfStrings
              return contexts

-- Parses a Task from (show Task), which is also how the Tasks are stored in the database file.
parseTask :: ReadP Task
parseTask = do
          tid <- parseId
          comma
          
          subject <- parseSubject
          comma
          
          projects <- parseProjects
          comma
          
          contexts <- parseContexts
          comma
          
          due <- parseDue  
          comma
          
          completed <- parseTaskStatus
          comma
          
          archived <- parseArchived
          comma
          
          priority <- parsePriority
          
          let newTask = Task tid subject projects contexts due completed archived priority 
          return newTask

parseTaskStatus :: ReadP TaskStatus
parseTaskStatus = do
          string "isComplete:"
          isComplete <- boolString
          let completed = if isComplete then Complete else Incomplete
          return completed

parseArchived :: ReadP Bool
parseArchived = do
          string "isArchived:"
          isArchived <- boolString
          return isArchived

parsePriority :: ReadP Priority
parsePriority = do
          string "isPriority:"
          isPriority <- boolString
          let priority = if isPriority then Urgent else Normal
          return priority
          
boolString :: ReadP Bool
boolString = do
           bString <- string "True" <|> string "False"
           let bVal = read bString :: Bool 
           return bVal

parseId :: ReadP Int
parseId = do 
        string "id:"
        tidString <- munch1 isNumber
        let tid = read tidString :: Int
        return tid

-- as noted elsewhere, this assumes the character '\"' is not part of the subject.
parseSubject :: ReadP String
parseSubject = do
          string "subject:"
          subject <- quotedString
          return subject

quotedString :: ReadP String
quotedString = do 
          quotes
          parsedString <- munch1 (not . isQuotes)
          quotes
          return parsedString

parseListOfStrings :: ReadP [String]
parseListOfStrings = do
                   openingBracket
                   strings <- quotedString `sepBy` comma
                   closingBracket
                   return strings

-- takes a list of serialized Task Strings, tries to parse into a list of Task.
parseDatabase :: [String] -> Maybe TaskList
parseDatabase [] = Just []
parseDatabase taskLines =  
                        -- each task is parsed into either a Just Task (if parsing succeeded) 
                        -- or Nothing (if parsing failed).
                        let parsedTasks = map (parseMaybe parseTask) taskLines
                        in sequenceA parsedTasks
                        -- now if all lines parsed correctly, we get Just TaskList. 
                        -- alternatively if any of the tasks failed to parse, result is Nothing. 

-- numbers taken from https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html
-- that tutorial was extremely helpful in understanding ReadP.
numbers :: Int -> ReadP Int
numbers numOfDigits = do
    parsedNumberString <- count numOfDigits digit
    
    let num = read parsedNumberString :: Int
    return num

--same as above but returns an Integer instead.
--needed because Day takes (Integer, Int, Int)
numbers' :: Int -> ReadP Integer
numbers' numOfDigits = do
    parsedNumberString <- count numOfDigits digit
    
    let num = read parsedNumberString :: Integer
    return num 

isNumber :: Char -> Bool
isNumber c = (c >= '0') && (c <= '9')

digit :: ReadP Char
digit = satisfy isNumber 

dateSeparator :: ReadP Char
dateSeparator = satisfy (\c -> c == '-')

maybeDayString :: ReadP (Maybe Day)
maybeDayString = do
           year <- numbers' 4
           dateSeparator
           day <- numbers 2
           dateSeparator
           month <- numbers 2
           
           let parsedDate = fromGregorian year day month
           return (Just parsedDate)

dayString :: ReadP Day
dayString = do
           year <- numbers' 4
           dateSeparator
           day <- numbers 2
           dateSeparator
           month <- numbers 2
           
           let parsedDate = fromGregorian year day month
           return parsedDate

parseDue :: ReadP (Maybe Day)
parseDue = do
         string "due:"

         --tries to parse as dayString, otherwise returns Nothing. kind of an ugly hack.
         due <- maybeDayString <++ (return Nothing) 
         
         return due

-- parseMaybe taken from https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html
parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

getToday :: IO Day
getToday = do
        curTime <- getCurrentTime
        let today = utctDay curTime
        return today

usageMessage :: String
usageMessage = "USAGE:" ++ "\n" 
                ++ "todo add TASK_NAME [due YYYY-MM-DD]" ++ "\n"
                ++ "todo complete [TASKNUM]" ++ "\n"
                ++ "todo delete [TASKNUM]" ++ "\n"
                ++ "todo urgent [TASKNUM]" ++ "\n"
                ++ "todo [archive/unarchive] [TASKNUM]"                             

addTask :: TaskList -> String -> Day -> TaskList 
addTask tasks args today =  let newId = findUnusedTaskIndex tasks
                            in let newTask = taskFromAddCommand args newId today
                               in case newTask of
                                  Nothing -> tasks
                                  Just task -> tasks ++ [task]

mutate :: TaskList -> Int -> (Task -> Task) -> TaskList
mutate [] _ _= []
mutate tasks taskNum action = let t = head tasks  
                              in if (tid t == taskNum) 
                                   then (action t) : (tail tasks)
                                   else (t : (mutate (tail tasks) taskNum action))


completeTask :: TaskList -> String -> Day -> TaskList 
completeTask tasks args _ =  let taskNum = parseMaybe mutatingCommand args
                             in case taskNum of
                                     Nothing -> tasks
                                     Just num -> mutate tasks num setComplete

setArchiveTask :: TaskList -> String -> Day -> TaskList 
setArchiveTask tasks args _ =  let taskNum = parseMaybe mutatingCommand args
                             in case taskNum of
                                     Nothing -> tasks
                                     Just num -> mutate tasks num setArchived

setUrgentTask :: TaskList -> String -> Day -> TaskList 
setUrgentTask tasks args _ =  let taskNum = parseMaybe mutatingCommand args
                             in case taskNum of
                                     Nothing -> tasks
                                     Just num -> mutate tasks num setUrgent 

setUnarchiveTask :: TaskList -> String -> Day -> TaskList 
setUnarchiveTask tasks args _ =  let taskNum = parseMaybe mutatingCommand args
                             in case taskNum of
                                     Nothing -> tasks
                                     Just num -> mutate tasks num setUnarchived


deleteTask :: TaskList -> String -> Day -> TaskList
deleteTask tasks args _ =  let taskNum = parseMaybe mutatingCommand args
                           in case taskNum of
                                   Nothing -> tasks
                                   Just num -> filter ( (/= num) . tid) tasks 

-- print all tasks (in the "formatted" view), but not archived tasks
printTasks :: TaskList -> Day -> IO ()
printTasks tasks today = let unarchived = filter (not . archived) tasks
                   in mapM_ (putStrLn . (formatTask today)) unarchived  

main :: IO ()
main = do  

    let dbFileName = "Tasks.db"

    handle <- openFile dbFileName ReadWriteMode
    dbContents <- hGetContents handle
    
    let linesToParse = lines dbContents

    let parsedTasks = parseDatabase linesToParse

    -- "safe" unpacking of the Maybe.
    when (parsedTasks == Nothing) 
      (die "Database parsing failed - maybe the file syntax is malformed.")

    let tasks = fromJust parsedTasks 
    
    today <- getToday

    args <- getArgs

    when (null args) (die usageMessage)

    let argCommand = head args
    
    unless (contains argCommand legalCommands) (die "Illegal command.")
    let command = fromJust (lookup argCommand commandToFunc)

    let argsString = intercalate " " args
    let tasksAfterCommand = command tasks argsString today

    printTasks tasksAfterCommand today

    handle <- openFile dbFileName WriteMode --deletes contents of file after opening

    let tasksToWrite = map show tasksAfterCommand

    hPutStr handle (intercalate "\n" tasksToWrite)

    hClose handle

    return ()
      