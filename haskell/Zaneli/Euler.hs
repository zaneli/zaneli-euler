module Zaneli.Euler (
  primesLimitIndex,
  primesLimitNum,
  divSum,
  divSum',
  primeFactors,
) where

-- n番目までの素数のリスト(降順)を返す
primesLimitIndex :: Integral a => Int -> [a]
primesLimitIndex n = primes (\_ list -> length list >= n)

-- n までの素数のリスト(降順)を返す
primesLimitNum :: Integral a => a -> [a]
primesLimitNum n = primes (\m _ -> m > n)

-- isExitを終了条件とする素数のリスト(降順)を返す
primes :: Integral a => (a -> [a] -> Bool) -> [a]
primes isExit = primes' 3 [2] []
    where
      -- mがlist'の要素のいずれでも割り切れない場合、mを素数とみなしてlistの先頭とlist'の末尾に追加する
      -- list'はisPrimeの処理高速化のため、昇順でのリストを保持する
      primes' m list list'
        | isExit m list = list
        | isPrime list' = primes' (m + 2) (m:list) (list' ++ [m])
        | otherwise     = primes' (m + 2) list list'
          where
            isPrime = all (\x -> m `mod` x /= 0) . takeWhile (\x -> x ^ 2 <= m)


-- 真の約数(その数自身を含まない約数)の和を返す
divSum :: Integral a => a -> a
divSum 1 = 0
divSum n = divSum' n - n

-- その数自身を含む約数の和を返す
divSum' :: Integral a => a -> a
divSum' = product . map (\(b, e) -> (b ^ (e + 1) - 1) `div` (b - 1)) . primeFactors

-- 因数分解した結果を(基数, 指数)のタプルのリストとして返す
primeFactors :: (Integral a, Num b) => a -> [(a, b)]
primeFactors n = primeFactors' n 2 []
  where
    primeFactors' n m list
      | n < m ^ 2     = updateList
      | isPrimeFactor =
          let (next, cnt) = divide n m 0 in
          primeFactors' next (m + 1) ((m, cnt):list)
      | otherwise     = primeFactors' n (m + 1) list
        where
          -- mがnを割り切れ、素数と判定済みのlistの中にmを割り切れる値がなければ、mを素数とみなす
          isPrimeFactor = all (\(p, _) -> m `mod` p /= 0) list && n `mod` m == 0
          -- n を m で割れるだけ割り、割れた回数とのタプルにして返す
          divide n m cnt | r == 0 && q /= 1 = divide q m (cnt + 1)
                         | otherwise        = (n, cnt)
                           where (q, r) = n `divMod` m
          -- n を(基数, 指数)のタプルのリストに追加する。基数として追加されていなければ追加し、追加されていれば指数に1加算したタプルに差し替える
          updateList | any (\(p, _) -> p == n) list = map (\(p, a) -> (p, if p == n then a + 1 else a)) list
                     | otherwise                    = (n, 1):list