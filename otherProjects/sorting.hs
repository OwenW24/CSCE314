-- bubble sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort' xs (length xs)
  where
    bubbleSort' lst 0 = lst
    bubbleSort' lst n = bubbleSort' (bubble lst) (n - 1)
    bubble (x:y:zs)
      | x > y     = y : bubble (x:zs)
      | otherwise = x : bubble (y:zs)
    bubble lst = lst

-- selection sort
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let minElem = minimum xs in minElem : selectionSort (removeFirst minElem xs)
  where
    removeFirst _ [] = []
    removeFirst y (z:zs)
      | y == z    = zs
      | otherwise = z : removeFirst y zs

-- insertion sort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

-- merge sort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- quick sort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]
