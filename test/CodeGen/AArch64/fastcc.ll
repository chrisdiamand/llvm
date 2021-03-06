; RUN: llc -verify-machineinstrs < %s -mtriple=aarch64-none-linux-gnu -tailcallopt | FileCheck %s -check-prefix CHECK-TAIL
; RUN: llc -verify-machineinstrs < %s -mtriple=aarch64-none-linux-gnu | FileCheck %s
; RUN: llc -verify-machineinstrs < %s -mtriple=aarch64-none-linux-gnu -tailcallopt -aarch64-redzone | FileCheck %s -check-prefix CHECK-TAIL-RZ

; Without tailcallopt fastcc still means the caller cleans up the
; stack, so try to make sure this is respected.

define fastcc void @func_stack0() {
; CHECK-LABEL: func_stack0:
; CHECK: mov x29, sp
; CHECK: str w{{[0-9]+}}, [sp, #-32]!

; CHECK-TAIL-LABEL: func_stack0:
; CHECK-TAIL: stp x29, x30, [sp, #-16]!
; CHECK-TAIL-NEXT: mov x29, sp
; CHECK-TAIL: str w{{[0-9]+}}, [sp, #-32]!


  call fastcc void @func_stack8([8 x i32] undef, i32 42)
; CHECK:  bl func_stack8
; CHECK-NOT: sub sp, sp,

; CHECK-TAIL: bl func_stack8
; CHECK-TAIL: sub sp, sp, #16


  call fastcc void @func_stack32([8 x i32] undef, i128 0, i128 9)
; CHECK: bl func_stack32
; CHECK-NOT: sub sp, sp,


; CHECK-TAIL: bl func_stack32
; CHECK-TAIL: sub sp, sp, #32


  call fastcc void @func_stack0()
; CHECK: bl func_stack0
; CHECK-NOT: sub sp, sp


; CHECK-TAIL: bl func_stack0
; CHECK-TAIL-NOT: sub sp, sp

  ret void
; CHECK: mov sp, x29
; CHECK-NEXT: ldp     x29, x30, [sp], #16
; CHECK-NEXT: ret


; CHECK-TAIL: mov sp, x29
; CHECK-TAIL-NEXT: ldp     x29, x30, [sp], #16
; CHECK-TAIL-NEXT: ret
}

define fastcc void @func_stack8([8 x i32], i32 %stacked) {
; CHECK-LABEL: func_stack8:
; CHECK: stp x29, x30, [sp, #-16]!
; CHECK: mov x29, sp
; CHECK: str w{{[0-9]+}}, [sp, #-32]!


; CHECK-TAIL-LABEL: func_stack8:
; CHECK-TAIL: stp x29, x30, [sp, #-16]!
; CHECK-TAIL: mov x29, sp
; CHECK-TAIL: str w{{[0-9]+}}, [sp, #-32]!


  call fastcc void @func_stack8([8 x i32] undef, i32 42)
; CHECK:  bl func_stack8
; CHECK-NOT: sub sp, sp,


; CHECK-TAIL: bl func_stack8
; CHECK-TAIL: sub sp, sp, #16


  call fastcc void @func_stack32([8 x i32] undef, i128 0, i128 9)
; CHECK: bl func_stack32
; CHECK-NOT: sub sp, sp,


; CHECK-TAIL: bl func_stack32
; CHECK-TAIL: sub sp, sp, #32


  call fastcc void @func_stack0()
; CHECK: bl func_stack0
; CHECK-NOT: sub sp, sp

; CHECK-TAIL: bl func_stack0
; CHECK-TAIL-NOT: sub sp, sp

  ret void
; CHECK: mov sp, x29
; CHECK-NEXT: ldp     x29, x30, [sp], #16
; CHECK-NEXT: ret


; CHECK-TAIL: mov sp, x29
; CHECK-TAIL-NEXT: ldp     x29, x30, [sp], #16
; CHECK-TAIL-NEXT: add     sp, sp, #16
; CHECK-TAIL-NEXT: ret
}

define fastcc void @func_stack32([8 x i32], i128 %stacked0, i128 %stacked1) {
; CHECK-LABEL: func_stack32:
; CHECK: mov x29, sp

; CHECK-TAIL-LABEL: func_stack32:
; CHECK-TAIL: mov x29, sp


  call fastcc void @func_stack8([8 x i32] undef, i32 42)
; CHECK:  bl func_stack8
; CHECK-NOT: sub sp, sp,

; CHECK-TAIL: bl func_stack8
; CHECK-TAIL: sub sp, sp, #16


  call fastcc void @func_stack32([8 x i32] undef, i128 0, i128 9)
; CHECK: bl func_stack32
; CHECK-NOT: sub sp, sp,


; CHECK-TAIL: bl func_stack32
; CHECK-TAIL: sub sp, sp, #32


  call fastcc void @func_stack0()
; CHECK: bl func_stack0
; CHECK-NOT: sub sp, sp


; CHECK-TAIL: bl func_stack0
; CHECK-TAIL-NOT: sub sp, sp

  ret void
; CHECK: mov sp, x29
; CHECK-NEXT: ldp     x29, x30, [sp], #16
; CHECK-NEXT: ret

; CHECK-TAIL: mov sp, x29
; CHECK-TAIL-NEXT: ldp     x29, x30, [sp], #16
; CHECK-TAIL-NEXT: add     sp, sp, #32
; CHECK-TAIL-NEXT: ret
}

; Check that arg stack pop is done after callee-save restore when no frame pointer is used.
define fastcc void @func_stack32_leaf([8 x i32], i128 %stacked0, i128 %stacked1) {
; CHECK-LABEL: func_stack32_leaf:
; CHECK: str     x20, [sp, #-16]!
; CHECK: nop
; CHECK-NEXT: //NO_APP
; CHECK-NEXT: ldr     x20, [sp], #16
; CHECK-NEXT: ret

; CHECK-TAIL-LABEL: func_stack32_leaf:
; CHECK-TAIL: str     x20, [sp, #-16]!
; CHECK-TAIL: nop
; CHECK-TAIL-NEXT: //NO_APP
; CHECK-TAIL-NEXT: ldr     x20, [sp], #16
; CHECK-TAIL-NEXT: add     sp, sp, #32
; CHECK-TAIL-NEXT: ret

; CHECK-TAIL-RZ-LABEL: func_stack32_leaf:
; CHECK-TAIL-RZ: str     x20, [sp, #-16]!
; CHECK-TAIL-RZ-NOT: sub     sp, sp
; CHECK-TAIL-RZ: nop
; CHECK-TAIL-RZ-NEXT: //NO_APP
; CHECK-TAIL-RZ-NEXT: ldr     x20, [sp], #16
; CHECK-TAIL-RZ-NEXT: add     sp, sp, #32
; CHECK-TAIL-RZ-NEXT: ret

  ; Make sure there is a callee-save register to save/restore.
  call void asm sideeffect "nop", "~{x20}"() nounwind
  ret void
}

; Check that arg stack pop is done after callee-save restore when no frame pointer is used.
define fastcc void @func_stack32_leaf_local([8 x i32], i128 %stacked0, i128 %stacked1) {
; CHECK-LABEL: func_stack32_leaf_local:
; CHECK: str     x20, [sp, #-16]!
; CHECK-NEXT: sub     sp, sp, #16
; CHECK: nop
; CHECK-NEXT: //NO_APP
; CHECK-NEXT: add     sp, sp, #16
; CHECK-NEXT: ldr     x20, [sp], #16
; CHECK-NEXT: ret

; CHECK-TAIL-LABEL: func_stack32_leaf_local:
; CHECK-TAIL: str     x20, [sp, #-16]!
; CHECK-TAIL-NEXT: sub     sp, sp, #16
; CHECK-TAIL: nop
; CHECK-TAIL-NEXT: //NO_APP
; CHECK-TAIL-NEXT: add     sp, sp, #16
; CHECK-TAIL-NEXT: ldr     x20, [sp], #16
; CHECK-TAIL-NEXT: add     sp, sp, #32
; CHECK-TAIL-NEXT: ret

; CHECK-TAIL-RZ-LABEL: func_stack32_leaf_local:
; CHECK-TAIL-RZ: str     x20, [sp, #-16]!
; CHECK-TAIL-RZ-NOT: sub     sp, sp
; CHECK-TAIL-RZ: nop
; CHECK-TAIL-RZ-NEXT: //NO_APP
; CHECK-TAIL-RZ-NEXT: ldr     x20, [sp], #16
; CHECK-TAIL-RZ-NEXT: add     sp, sp, #32
; CHECK-TAIL-RZ-NEXT: ret

  %val0 = alloca [2 x i64], align 8

  ; Make sure there is a callee-save register to save/restore.
  call void asm sideeffect "nop", "~{x20}"() nounwind
  ret void
}

; Check that arg stack pop is done after callee-save restore when no frame pointer is used.
define fastcc void @func_stack32_leaf_local_nocs([8 x i32], i128 %stacked0, i128 %stacked1) {
; CHECK-LABEL: func_stack32_leaf_local_nocs:
; CHECK: sub     sp, sp, #16
; CHECK: add     sp, sp, #16
; CHECK-NEXT: ret

; CHECK-TAIL-LABEL: func_stack32_leaf_local_nocs:
; CHECK-TAIL: sub     sp, sp, #16
; CHECK-TAIL: add     sp, sp, #48
; CHECK-TAIL-NEXT: ret

; CHECK-TAIL-RZ-LABEL: func_stack32_leaf_local_nocs:
; CHECK-TAIL-RZ: add     sp, sp, #32
; CHECK-TAIL-RZ-NEXT: ret

  %val0 = alloca [2 x i64], align 8

  ret void
}
