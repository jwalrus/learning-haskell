-- candidate assessment with monads
-- Get Programming with Haskell (Kurt), Lesson 31
import qualified Data.Map as Map

-- MODEL
data Grade = F | D | C | B | A deriving (Show, Read, Ord, Enum, Eq)
data Degree = HS | BA | MS | PhD deriving (Show, Read, Ord, Enum, Eq)

data Candidate = Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree
    }

-- DATA
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

candidateDb :: Map.Map Int Candidate
candidateDb = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]


-- FUNCTIONS
viable :: Candidate -> Bool
viable candidate = all (==True) tests
    where passedCodeReview = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMinimum = education candidate >= MS
          tests = [passedCodeReview, passedCultureFit, educationMinimum]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
   putStrLn "enter id:"
   cId <- readInt
   putStrLn "enter code grade:"
   codeGrade <- readGrade
   putStrLn "enter culture fit grade:"
   cultureGrade <- readGrade
   putStrLn "enter education:"
   degree <- readDegree
   return (Candidate { candidateId = cId
                     , codeReview = codeGrade
                     , cultureFit = cultureGrade
                     , education = degree })

-- ENTRY POINT
assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed 
                    then "passed"
                    else "failed"
    return statement

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe id = do
    candidate <- Map.lookup id candidateDb 
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

assessCandidateMonad :: (Monad m) => m Candidate -> m String
assessCandidateMonad candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

