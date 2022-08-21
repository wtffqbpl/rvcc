# return specified number
add_test(NAME specified_num_1 COMMAND rvcc "{ return 0; }")
add_test(NAME specified_num_2 COMMAND rvcc "{ return 2; }")
add_test(NAME specified_num_3 COMMAND rvcc "{ return 42; }")

# [2] support +/- operators.
add_test(NAME neg_pos_1 COMMAND rvcc "{ return 12-34+56; }")

# [3] support spaces.
add_test(NAME spaces_1 COMMAND rvcc "{ return 12 - 34 + 56; }")

# [5] support * / () operators.
add_test(NAME math_op_1 COMMAND rvcc "{ return 5 + 6 * 7; }")
add_test(NAME math_op_2 COMMAND rvcc "{ return 5 * (9 - 6); }")
add_test(NAME math_op_3 COMMAND rvcc "{ return 1 - 8 / (2 * 2) + 3 * 6; }")

# [6] support unary operator +/- (positive/neg)
add_test(NAME unary_op_1 COMMAND rvcc "{ return -10 + 20; }")
add_test(NAME unary_op_2 COMMAND rvcc "{ return - -10; }")
add_test(NAME unary_op_3 COMMAND rvcc "{ return - - +10; }")
add_test(NAME unary_op_4 COMMAND rvcc "{ return ------12*+++++----++++++++++4; }")

# [7] support conditional operator.
add_test(NAME conditional_op_1 COMMAND rvcc "{ return 0==1; }")
add_test(NAME conditional_op_2 COMMAND rvcc "{ return 42==42; }")
add_test(NAME conditional_op_3 COMMAND rvcc "{ return 0!=1; }")
add_test(NAME conditional_op_4 COMMAND rvcc "{ return 42!=42; }")
add_test(NAME conditional_op_5 COMMAND rvcc "{ return 0<1; }")
add_test(NAME conditional_op_6 COMMAND rvcc "{ return 1<1; }")
add_test(NAME conditional_op_7 COMMAND rvcc "{ return 2<1; }")
add_test(NAME conditional_op_8 COMMAND rvcc "{ return 0<=1; }")
add_test(NAME conditional_op_9 COMMAND rvcc "{ return 1<=1; }")
add_test(NAME conditional_op_10 COMMAND rvcc "{ return 2<=1; }")
add_test(NAME conditional_op_11 COMMAND rvcc "{ return 1>0; }")
add_test(NAME conditional_op_12 COMMAND rvcc "{ return 1>1; }")
add_test(NAME conditional_op_13 COMMAND rvcc "{ return 1>2; }")
add_test(NAME conditional_op_14 COMMAND rvcc "{ return 1>=0; }")
add_test(NAME conditional_op_15 COMMAND rvcc "{ return 1>=1; }")
add_test(NAME conditional_op_16 COMMAND rvcc "{ return 1>=2; }")
add_test(NAME conditional_op_17 COMMAND rvcc "{ return 5==2+3; }")
add_test(NAME conditional_op_18 COMMAND rvcc "{ return 6==4+3; }")
add_test(NAME conditional_op_19 COMMAND rvcc "{ return 0*9+5*2==4+4*(6/3)-2; }")

# [9] support ; separator.
add_test(NAME separator_1 COMMAND rvcc "{ 1; 2;return 3; }")
add_test(NAME separator_2 COMMAND rvcc "{ 12+23;12+99/3;return 78-66; }")

# [10] support single charactor variable name.
add_test(NAME single_char_var_name_1 COMMAND rvcc "{ a=3;return a; }")
add_test(NAME single_char_var_name_2 COMMAND rvcc "{ a=3; z=5;return a+z; }")
add_test(NAME single_char_var_name_3 COMMAND rvcc "{ a=b=3;return a+b; }")
add_test(NAME single_char_var_name_4 COMMAND rvcc "{ a=3;b=4;a=1;return a+b; }")

# [11] support return
add_test(NAME return_1 COMMAND rvcc "{ return 1; 2; 3; }")
add_test(NAME return_2 COMMAND rvcc "{ 1; return 2; 3; }")
add_test(NAME return_3 COMMAND rvcc "{ 1; 2; return 3; }")

# [12] support multiple-character variable name.
add_test(NAME multiple_char_var_name_1 COMMAND rvcc "{ foo=3;return foo; }")
add_test(NAME multiple_char_var_name_2 COMMAND rvcc "{ foo2=70; bar4=4;return foo2+bar4; }")

# [13] support {...}
add_test(NAME block_code_1 COMMAND rvcc "{ 1; {2;} return 3; }")

# [14] support empty statements.
add_test(NAME empty_1 COMMAND rvcc "{ ; ; ; return 1 + 2; }")

# [15] support if statement.
add_test(NAME if_1 COMMAND rvcc "{ if (0) return 2; return 3; }")
add_test(NAME if_2 COMMAND rvcc "{ if (1-1) return 2; return 3; }")
add_test(NAME if_3 COMMAND rvcc "{ if (1) return 2; return 3; }")
add_test(NAME if_4 COMMAND rvcc "{ if (2-1) return 2; return 3; }")
add_test(NAME if_5 COMMAND rvcc "{ if (0) { 1; 2; return 3; } else { return 4; } }")
add_test(NAME if_6 COMMAND rvcc "{ if (1) { 1; 2; return 3; } else { return 4; } }")

# [16] support for statement.
add_test(NAME for_1 COMMAND rvcc "{ i=0; j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }")
add_test(NAME for_2 COMMAND rvcc "{ for (;;) {return 3;} return 5; }")

# [17] support while statement.
add_test(NAME while_1 COMMAND rvcc "{ i=0; while(i<10) { i=i+1; } return i; }")

# [20] support unary operator & * (acquire address de de-reference address)
add_test(NAME addr_op_1 COMMAND rvcc "{ x=3; return *&x; }")
add_test(NAME addr_op_2 COMMAND rvcc "{ x=3; y=&x; z=&y; return **z; }")
add_test(NAME addr_op_3 COMMAND rvcc "{ x=3; y=5; return *(&x+8); }")
add_test(NAME addr_op_4 COMMAND rvcc "{ x=3; y=5; return *(&y-8); }")
add_test(NAME addr_op_5 COMMAND rvcc "{ x=3; y=&x; *y=5; return x; }")
add_test(NAME addr_op_6 COMMAND rvcc "{ x=3; y=5; *(&x+8)=7; return y; }")
add_test(NAME addr_op_7 COMMAND rvcc "{ x=3; y=5; *(&y-8)=7; return x; }")