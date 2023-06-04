// RUN: ./scriptlang %s | FileCheck %s

// CHECK-LABEL: define i32 @main(i32 %0, ptr %1)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %2 = alloca ptr, align 8
// CHECK-NEXT:    store ptr @"lambda#0", ptr %2, align 8
// CHECK-NEXT:    %3 = alloca ptr, align 8
// CHECK-NEXT:    store ptr @"lambda#1", ptr %3, align 8

// CHECK-LABEL: define void @"lambda#0"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %0 = alloca i32, align 4
// CHECK-NEXT:    store i32 1, ptr %0, align 4
let fn_normal = () => {
  let a = 1;
};

// CHECK-LABEL: define void @"lambda#1"(i32 %0) {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %1 = alloca i32, align 4
// CHECK-NEXT:    store i32 2, ptr %1, align 4
let fn_argument = (a: i32) => {
  let b = 2;
};

// CHECK-LABEL: define i32 @"lambda#2"() {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %0 = alloca i32, align 4
// CHECK-NEXT:    store i32 3, ptr %0, align 4
// CHECK-NEXT:    %1 = load i32, ptr %0, align 4
// CHECK-NEXT:    ret i32 %1
let fn_return = () => {
  let c = 3;
  return c;
};

// CHECK-LABEL: define i32 @"lambda#3"() {
// CHECK-NEXT:  entry:
// CHECK-NEXT:    %0 = alloca i32, align 4
// CHECK-NEXT:    store i32 4, ptr %0, align 4
// CHECK-NEXT:    %1 = load i32, ptr %0, align 4
// CHECK-NEXT:    ret i32 %1
let fn_same_name = () => {
  let c = 4;
  return c;
};

// CHECK-LABEL: define void @fn_fix_name() {
// CHECK-NEXT:  entry:
const fn_fix_name = () => {};
