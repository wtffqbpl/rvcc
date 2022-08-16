# return specified number
add_test(NAME 1_1 COMMAND rvcc "{ return 0; }")
add_test(NAME 1_2 COMMAND rvcc "{ return 2; }")
add_test(NAME 1_3 COMMAND rvcc "{ return 42; }")

# [2] support +/- operators.
add_test(NAME 2_1 COMMAND rvcc "{ return 12-34+56; }")

# [3] support spaces.
add_test(NAME 3_1 COMMAND rvcc "{ return 12 - 34 + 56; }")

# [5] support * / () operators.
add_test(NAME 5_1 COMMAND rvcc "{ return 5 + 6 * 7; }")
add_test(NAME 5_2 COMMAND rvcc "{ return 5 * (9 - 6); }")
add_test(NAME 5_3 COMMAND rvcc "{ return 1 - 8 / (2 * 2) + 3 * 6; }")

# [6] support unary operator +/- (positive/neg)
add_test(NAME 6_1 COMMAND rvcc "{ return -10 + 20; }")
add_test(NAME 6_2 COMMAND rvcc "{ return - -10; }")
add_test(NAME 6_3 COMMAND rvcc "{ return - - +10; }")
add_test(NAME 6_4 COMMAND rvcc "{ return ------12*+++++----++++++++++4; }")

# [7] support conditional operator.
add_test(NAME 7_1 COMMAND rvcc "{ return 0==1; }")
add_test(NAME 7_2 COMMAND rvcc "{ return 42==42; }")
add_test(NAME 7_3 COMMAND rvcc "{ return 0!=1; }")
add_test(NAME 7_4 COMMAND rvcc "{ return 42!=42; }")
add_test(NAME 7_5 COMMAND rvcc "{ return 0<1; }")
add_test(NAME 7_6 COMMAND rvcc "{ return 1<1; }")
add_test(NAME 7_7 COMMAND rvcc "{ return 2<1; }")
add_test(NAME 7_8 COMMAND rvcc "{ return 0<=1; }")
add_test(NAME 7_9 COMMAND rvcc "{ return 1<=1; }")
add_test(NAME 7_10 COMMAND rvcc "{ return 2<=1; }")
add_test(NAME 7_11 COMMAND rvcc "{ return 1>0; }")
add_test(NAME 7_12 COMMAND rvcc "{ return 1>1; }")
add_test(NAME 7_13 COMMAND rvcc "{ return 1>2; }")
add_test(NAME 7_14 COMMAND rvcc "{ return 1>=0; }")
add_test(NAME 7_15 COMMAND rvcc "{ return 1>=1; }")
add_test(NAME 7_16 COMMAND rvcc "{ return 1>=2; }")
add_test(NAME 7_17 COMMAND rvcc "{ return 5==2+3; }")
add_test(NAME 7_18 COMMAND rvcc "{ return 6==4+3; }")
add_test(NAME 7_19 COMMAND rvcc "{ return 0*9+5*2==4+4*(6/3)-2; }")

# [9] support ; separator.
add_test(NAME 9_1 COMMAND rvcc "{ 1; 2;return 3; }")
add_test(NAME 9_2 COMMAND rvcc "{ 12+23;12+99/3;return 78-66; }")

# [10] support single charactor variable name.
add_test(NAME 10_1 COMMAND rvcc "{ a=3;return a; }")
add_test(NAME 10_2 COMMAND rvcc "{ a=3; z=5;return a+z; }")
add_test(NAME 10_3 COMMAND rvcc "{ a=b=3;return a+b; }")
add_test(NAME 10_4 COMMAND rvcc "{ a=3;b=4;a=1;return a+b; }")

# [11] support return
add_test(NAME 11_1 COMMAND rvcc "{ return 1; 2; 3; }")
add_test(NAME 11_2 COMMAND rvcc "{ 1; return 2; 3; }")
add_test(NAME 11_3 COMMAND rvcc "{ 1; 2; return 3; }")

# [12] support multiple-character variable name.
add_test(NAME 12_1 COMMAND rvcc "{ foo=3;return foo; }")
add_test(NAME 12_2 COMMAND rvcc "{ foo2=70; bar4=4;return foo2+bar4; }")

# [13] support {...}
add_test(NAME 13_1 COMMAND rvcc "{ 1; {2;} return 3; }")

# [14] support empty statements.
add_test(NAME 14_1 COMMAND rvcc "{ ; ; ; return 1 + 2; }")

# [15] support if statement.
add_test(NAME 15_1 COMMAND rvcc "{ if (0) return 2; return 3; }")
add_test(NAME 15_2 COMMAND rvcc "{ if (1-1) return 2; return 3; }")
add_test(NAME 15_3 COMMAND rvcc "{ if (1) return 2; return 3; }")
add_test(NAME 15_4 COMMAND rvcc "{ if (2-1) reutrn 2; return 3; }")
add_test(NAME 15_5 COMMAND rvcc "{ if (0) { 1; 2; return 3; } else { return 4; }")
add_test(NAME 15_6 COMMAND rvcc "{ if (1) { 1; 2; return 3; } else { return 4; }")