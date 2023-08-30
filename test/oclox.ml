open OUnit2

let rec sum = function
    | [] -> 0
    | x :: xs -> x + sum xs

let tests = "test suite for sum" >::: [
    "empty" >:: (fun _ -> assert_equal 0 (sum []) ~printer:string_of_int);
    "singleton" >:: (fun _ -> assert_equal 1 (sum [1]) ~printer:string_of_int);
    "two_elements" >:: (fun _ -> assert_equal 3 (sum [1; 2]) ~printer:string_of_int);
]

let _ = run_test_tt_main tests
