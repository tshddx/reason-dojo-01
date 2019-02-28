# 1 "result-as-newtype.ml"
type ('a, 'b) result = Ok of 'a | Error of 'b
