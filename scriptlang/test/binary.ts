// RUN: ./scriptlang %s | FileCheck %s

// CHECK-LABEL: main
// CHECK-NEXT:  entry:
// CHECK-NEXT:  %2 = alloca i32, align 4
// CHECK-NEXT:  store i32 3, ptr %2, align 4
// CHECK-NEXT:  %3 = alloca i32, align 4
// CHECK-NEXT:  store i32 7, ptr %3, align 4
// CHECK-NEXT:  %4 = load i32, ptr %2, align 4
// CHECK-NEXT:  %5 = load i32, ptr %3, align 4
// CHECK-NEXT:  %6 = mul i32 %4, %5
// CHECK-NEXT:  %7 = alloca i32, align 4
// CHECK-NEXT:  store i32 %6, ptr %7, align 4

let add = 1 + 2;
const dec = 3 + 4;
let mul_result = add * dec;
