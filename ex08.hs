-- partition 
partition :: (a -> Bool) -> [a] -> ([a], [a]) -- the type of partition
partition p [] = ([], [])
partition p (x:xs)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition p xs
-- what does partition do?
--      Splits a list into two lists based on a given predicate. 
--      If an element satisfies the predicate, it goes into the first list, else it goes into the second list.

-- rewrite filter using partition
filter' :: (a -> Bool) -> [a] -> [a]
filter' p ls = fst(partition p ls)

--rewrite quicksort without using list comprehension
qsort :: Ord a => [a] -> [a]
qsort []      = []
qsort (hd:tl) =
    qsort l ++ [hd] ++ qsort r
    where (l, r) = partition (< hd) tl

-- look up type Ordering what is it for? 
--      type Ordering :: *
--      Ordering is used to indicate the result of comparing two values. 
--      It determines whether one value is less than, equal to, or greater than another value. 
-- how many constructors are there? 
--      3 (LT | EQ | GT)
-- how many ways can we pattern-match an Ordering value?
--      3 ways corresponding to its three constructors.

-- write function gpa that computes grade-point average from a given list of letter grades
gradeToPoint :: String -> Double
gradeToPoint "A"  = 4.0
gradeToPoint "B+" = 3.5
gradeToPoint "B"  = 3.0
gradeToPoint "C+" = 2.5
gradeToPoint "C"  = 2.0
gradeToPoint "D+" = 1.5
gradeToPoint "D"  = 1.0
gradeToPoint "F"  = 0.0
gradeToPoint _    = error "Invalid grade"

gpa :: [String] -> Double
gpa grades 
    | null listOfGrades = 0.0
    | otherwise = sum listOfGrades / fromIntegral (length listOfGrades)
    where listOfGrades = map gradeToPoint (filter (/= "W") grades)
