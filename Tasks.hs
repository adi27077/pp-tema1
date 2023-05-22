-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : map compute_average_row (tail m)

--functii care calculeaza media de pasi unei linii
compute_average_row :: Row -> Row
compute_average_row r = head r : [compute_average r]

compute_average :: Row -> Value
compute_average r = printf "%.2f" ((read (foldr compute_sum "0" (tail r)) :: Float) / 8)

--functie ajutatoare pt fold care calculeaza suma
compute_sum :: Value -> Value -> Value
compute_sum v acc = show (read v + read acc)

-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr (check_achieve . compute_sum_row) 0 (tail m)

compute_sum_row :: Row -> Row
compute_sum_row r = [foldr compute_sum "0" (tail r)]

--functie ajutatoare pt fold folosita pt numararea persoanelor
--care au indeplinit conditia de pasi
check_achieve :: Row -> Int -> Int
check_achieve v acc
              | read (head v) >= 1000 = acc + 1
              | otherwise = acc


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = fromIntegral (get_passed_people_num m) / get_total_people_num m

--functie care calculeaza nr total al persoanelor din tabel
get_total_people_num :: Table -> Float
get_total_people_num m = foldr (\v acc -> acc + 1) 0.0 (tail m)


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = foldr ((\v acc -> acc + read (head v)) . compute_sum_row) 0 (tail m) / get_total_people_num m




-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : [f_aux get_avg_steps_hour (map tail (tail m)) []]

get_avg_steps_hour :: Table -> Value
get_avg_steps_hour t = printf "%.2f" ((read (foldr (\v acc -> show (read (head v) + read acc)) "0" t) :: Float) / (get_total_people_num t + 1))

--functie auxiliara care construieste tabelul cerut
f_aux :: (Table -> Value) -> Table -> Row -> Row
f_aux f ([]:_) acc = acc
f_aux f t acc = acc ++ [f t] ++ f_aux f (map tail t) acc



-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] : compute_minutes (map (drop 3) (tail m))

compute_minutes :: Table -> Table
compute_minutes t = [comp_vact_min t] ++ [comp_fair_min (map tail t)] ++ [comp_light_min (map (drop 2) t)]

--functii auxiliare care construiesc randurile pt fiecare categorie
comp_vact_min :: Table -> Row
comp_vact_min t = ["VeryActiveMinutes"] ++ [comp_low t] ++ [comp_med t] ++ [comp_high t]

comp_fair_min :: Table -> Row
comp_fair_min t = ["FairlyActiveMinutes"] ++ [comp_low t] ++ [comp_med t] ++ [comp_high t]

comp_light_min :: Table -> Row
comp_light_min t = ["LightlyActiveMinutes"] ++ [comp_low t] ++ [comp_med t] ++ [comp_high t]

--functii care numara persoanele din fiecare categorie
comp_low :: Table -> Value
comp_low = foldr check_low_range "0"

comp_med :: Table -> Value
comp_med = foldr check_med_range "0"

comp_high :: Table -> Value
comp_high = foldr check_high_range "0"

--functiile folosite de fold pt numararea persoanelor din fiecare range
check_low_range :: Row -> Value -> Value
check_low_range r acc
       | read (head r) < 50 = show (read acc + 1)
       | otherwise = acc

check_med_range :: Row -> Value -> Value
check_med_range r acc
       | read (head r) >= 50 && read (head r) < 100 = show (read acc + 1)
       | otherwise = acc

check_high_range :: Row -> Value -> Value
check_high_range r acc
       | read (head r) >= 100 && read (head r) < 500 = show (read acc + 1)
       | otherwise = acc


-- Task 5

get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : sortBy sort_cmp  (map (reverse . drop 4 . reverse)  (tail m))

--comparator pt sort
sort_cmp :: Row -> Row -> Ordering
sort_cmp a b
        | head (tail a) < head (tail b) = GT
        | head (tail a) > head (tail b) = LT
        | head (tail a) == head (tail b) && head a < head b = GT
        | head (tail a) == head (tail b) && head a > head b = LT
        | otherwise = EQ

-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : sort_step_diff (tail m)

sort_step_diff :: Table -> Table
sort_step_diff m = sortBy sort_cmp_diff (map (compute_diff_row . compute_average4_row) m)

--functie care construieste linia cu cele 2 medii pe 4 ore
compute_average4_row :: Row -> Row
compute_average4_row r = [head r] ++ [average_4h (tail r)] ++ [average_4h (drop 5 r)]

--functie care calculeaza media pe 4 ore
average_4h :: Row -> Value
average_4h r = printf "%.2f" (((read (head r) :: Float) + (read (r !! 1) :: Float) + (read (r !! 2) :: Float) + (read (r !! 3) :: Float)) / 4)

--functie care adauga la linie diferenta dintre cele 2 medii
compute_diff_row :: Row -> Row
compute_diff_row r = r ++ [printf "%.2f" (abs ((read (r !! 1) :: Float) - (read (r !! 2) :: Float)))]

--comparator pt sort
sort_cmp_diff :: Row -> Row -> Ordering
sort_cmp_diff a b
         | (read (a !! 3) :: Float) < (read (b !! 3) :: Float) = GT
         | (read (a !! 3) :: Float) > (read (b !! 3) :: Float) = LT
         | (read (a !! 3) :: Float) == (read (b !! 3) :: Float) && head a < head b = GT
         | (read (a !! 3) :: Float) == (read (b !! 3) :: Float) && head a > head b = LT
         | otherwise = EQ



-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map f m


get_sleep_total :: Row -> Row
get_sleep_total r = head r : [printf "%.2f" (read (foldr (\v acc -> show (read acc + read v)) "0" (tail r)) :: Float)]
