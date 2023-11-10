module Main where

{-
    Day 13

      Part A
      . Given a list of pairs of packets; pairs are separated by a blank line.
      . Identify how many pairs of packets are in the right order via these rules:

      . Packet data consists of lists and integers. Each list starts with [, ends with ],
        and >= 0 CSV (either integers or other lists).
      . Each packet is always a list and appears on its own line.

      . When comparing 2 values, the 1st value is called "left" and the 2nd value, "right".
      . Then:

        . If both values are integers, the lower integer should come first.
          If the left < right, the inputs are in the right order.
          If the left > right, the inputs are not in the right order.
          Otherwise, the inputs are the same integer;
          continue checking the next part of the input.
        . If both values are lists, compare the first value of each list,
          then the second value, and so on. If the left list runs out of items first,
          the inputs are in the right order. If the right list runs out of items
          first, the inputs are not in the right order. If the lists are the same
          length and no comparison makes a decision about the order, continue checking
          the next part of the input.
        . If exactly one value is an integer, convert the integer to a list which
          contains that integer as its only value, then retry the comparison. For
          example, if comparing [0,0,0] and 2, convert the right value to [2]
          (a list containing 2); the result is then found by instead comparing
          [0,0,0] and [2].

      . What are the indices of the pairs that are already in the right order?
        (The first pair has index 1, the second pair has index 2, and so on.)

      . Determine which pairs of packets are already in the right order.
        What is the sum of the indices of those pairs?

      . Using the example (input-13-test.txt), the pairs in the right order are:
        1, 2, 4, and 6;
        the sum of these indices is 13.
-}

main :: IO ()
main = do
  putStrLn "Day 13 - Part A"
