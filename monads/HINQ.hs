-- HINQ - SQL like queries in Haskell
-- Get Programming with Haskell (Kurt), Lesson 33
import Control.Monad

-- MODEL
data Name = Name
    { firstName :: String
    , lastName :: String
    }

instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman 
    | Sophomore
    | Junior
    | Senior
    | SuperSenior deriving (Eq, Ord, Enum, Show)

data Student = Student
    { studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name
    } deriving Show

data Teacher = Teacher
    { teacherId :: Int
    , teacherName :: Name
    } deriving Show

data Course = Course
    { courseId :: Int
    , courseTitle :: String
    , teacherId :: Int
    } deriving Show


-- DATA
students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
           ,(Student 2 Junior (Name "Leslie" "Silko"))
           ,(Student 3 Freshman (Name "Judith" "Butler"))
           ,(Student 4 Senior (Name "Guy" "Debord"))
           ,(Student 5 Sophomore (Name "Jean" "Baudrillard"))
           ,(Student 6 Junior (Name "Julia" "Kristeva"))]

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
           ,Teacher 200 (Name "Susan" "Sontag")]

courses :: [Course]
courses = [Course 101 "French" 100
          ,Course 201 "English" 200]


-- FUNCTIONS
_select :: (a -> b) -> [a] -> [b]
_select f xs = do
    x <- xs
    return $ f x

_where :: (a -> Bool) -> [a] -> [a]
_where pred xs = do
    x <- xs
    guard(pred x)
    return x

startsWith :: Char -> String -> Bool
startsWith char str = char == (head str)

-- todo: start at implementing _join
