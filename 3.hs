--------------------
------ 3 -----------
--------------------
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

factors n = map fst (filter areFactorsOfN (join (map (\x -> map (\y -> (x,y)) [1..n]) [1..n])))
        where areFactorsOfN (x, y) = x * y == n
factorsWithoutOneAndN n = filter (\x -> not (x == 1) && not (x == n)) (factors n)
isPrime 1 = True
isPrime 2 = True
isPrime n = if (even n) then False else null (factorsWithoutOneAndN n)
primeFactors n = filter isPrime (factorsWithoutOneAndN n)

primeTests = runTests [
        Test "isPrime 1" True (isPrime 1),
        Test "isPrime 6" False (isPrime 6),
        Test "isPrime 7" True (isPrime 7),
        Test "isPrime 12" False (isPrime 12),
        Test "isPrime 13" True (isPrime 13)
        ]

factorsTests = runTests [
        Test "factors of 4" [1,2,4] (factors 4),
        Test "factors of 4" [1,2,3,6] (factors 6),
        Test "factors of 20" [1,2,4,5,10,20] (factors 20),
        Test "factors of 99" [1,3,9,11,33,99] (factors 99)
        ]

primeFactorsTests = runTests [
        Test "prime factors of 99" [3,11] (primeFactors 99),
        Test "prime factors of 13195" [5,7,13,29] (primeFactors 13195)]

--actual3 = (max (primeFactors 600851475143))
--expected3 = 600851475143
--problem3Tests = runTests [Test "3" expected3 actual3]

