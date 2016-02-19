; RUN: llc -march=sparc < %s | FileCheck %s
declare void @stack_realign_helper(i32 %a, i32* %b)

@foo = global i32 1

;; This is a function where we have a local variable of 64-byte
;; alignment.  We want to see that the stack is aligned (the initial
;; andn), that the local var is accessed via stack pointer (to %o0), and that
;; the argument is accessed via frame pointer not stack pointer (to %o1).

;; CHECK-LABEL: stack_realign:
;; CHECK:      andn %sp, 63, %sp
;; CHECK-NEXT: ld [%fp+92], %o0
;; CHECK-NEXT: call stack_realign_helper
;; CHECK-NEXT: add %sp, 128, %o1

define void @stack_realign(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e, i32 %f, i32 %g) {
entry:
  %aligned = alloca i32, align 64
  call void @stack_realign_helper(i32 %g, i32* %aligned)
  ret void
}
