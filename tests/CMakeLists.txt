# return specified number
add_test(NAME return_1 COMMAND rvcc "return 1; 2; 3;")
add_test(NAME return_2 COMMAND rvcc "1; return 2; 3;")
add_test(NAME return_3 COMMAND rvcc "1; 2; return 3;")
