define i64 @program(i64 %argc, i8** %arcv) {
  %1 = add i64 7, 7
  br label %l2
l2:
  %2 = mul i64 2, %1
  br label %l3
l3:
  %3 = sub i64 %2, 32
  br label %l4
l4:
  %4 = shl i64 %3, 1
  br label %l5
l5:
  %5 = lshr i64 %4, 60
  br label %l6
l6:
  %6 = ashr i64 %5, 2
  br label %l7
l7:
  %7 = and i64 255, %6
  br label %l8
l8:
  %8 = or i64 64, %7
  br label %l9
l9:
  %9 = xor i64 %8, 255
  br label %lexit
lexit:
  ret i64 %9
}

