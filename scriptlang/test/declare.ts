// RUN: ./scriptlang %s | FileCheck %s

// CHECK-LABEL: main
// CHECK-NEXT:  entry:
// CHECK-NEXT:  %2 = alloca i32, align 4
// CHECK-NEXT:  store i32 1, ptr %2, align 4
// CHECK-NEXT:  %3 = alloca i32, align 4
// CHECK-NEXT:  store i32 2, ptr %3, align 4
// CHECK-NEXT:  %4 = alloca i32, align 4
// CHECK-NEXT:  store i32 -1, ptr %4, align 4
// CHECK-NEXT:  %5 = alloca float, align 4
// CHECK-NEXT:  store float 1.000000e+00, ptr %5, align 4
// CHECK-NEXT:  %6 = alloca double, align 8
// CHECK-NEXT:  store float 2.000000e+00, ptr %6, align 4
let num1 = 1;
const num2 = 2;
let num3 = -1;

let float1 = 1.0;
let float2: f64 = 2.0;
