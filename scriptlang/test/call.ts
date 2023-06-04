// RUN: ./scriptlang %s | FileCheck %s

// CHECK-LABEL: define i32 @main(i32 %0, ptr %1) {
// CHECK-NEXT:  entry:
// CHECK-NEXT:  %2 = alloca ptr, align 8
// CHECK-NEXT:  store ptr @called_func, ptr %2, align 8
// CHECK-NEXT:  %3 = load ptr, ptr %2, align 8
// CHECK-NEXT:  call void %3()
// CHECK-NEXT:  %4 = alloca ptr, align 8
// CHECK-NEXT:  store ptr @call_ed_func_with_args, ptr %4, align 8
// CHECK-NEXT:  %5 = load ptr, ptr %4, align 8
// CHECK-NEXT:  call void %5(i32 1)
// CHECK-NEXT:  %6 = alloca ptr, align 8
// CHECK-NEXT:  store ptr @call_ed_func_with_return, ptr %6, align 8
// CHECK-NEXT:  %7 = load ptr, ptr %6, align 8
// CHECK-NEXT:  %8 = call i32 %7()
// CHECK-NEXT:  %9 = alloca i32, align 4
// CHECK-NEXT:  store i32 %8, ptr %9, align 4

const called_func = () => {};
called_func();

const call_ed_func_with_args = (a: i32) => {};
call_ed_func_with_args(1);

const call_ed_func_with_return = () => {
  return 1;
};
const return_value = call_ed_func_with_return();
