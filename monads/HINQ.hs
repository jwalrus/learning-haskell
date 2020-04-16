-- HINQ - SQL like queries in Haskell
-- Get Programming with Haskell (Kurt), Lesson 33
import Control.Monad
import Control.Applicative

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
    , teacher :: Int
    } deriving Show

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)


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
_select :: (Monad m) => (a -> b) -> m a -> m b
_select f xs = do
    x <- xs
    return $ f x

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where pred xs = do
    x <- xs
    guard(pred x)
    return x

startsWith :: Char -> String -> Bool
startsWith char str = char == (head str)

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join xs ys fx fy = do
    x <- xs
    y <- ys
    guard((fx x) == (fy y))
    return (x,y)

_hinq selectQuery joinQuery whereQuery = (\joinData -> 
                                                (\whereResult ->
                                                    selectQuery whereResult)
                                                (whereQuery joinData)
                                            ) joinQuery

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sc jc wc) = _hinq sc jc wc
runHINQ (HINQ_ sc jc) = _hinq sc jc (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((=="English") . courseTitle .snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
               teachers

-- extending to other monads
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherId teacher)
                   (_where ((=="French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

missingQuery1 :: HINQ Maybe (Teacher, Course) Name
missingQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((=="French") . courseTitle . snd))

-- pick up at 33.6.2
