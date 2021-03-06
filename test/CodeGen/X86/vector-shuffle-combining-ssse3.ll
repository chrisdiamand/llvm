; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-unknown -mattr=+ssse3 | FileCheck %s --check-prefix=ALL --check-prefix=SSE --check-prefix=SSSE3
; RUN: llc < %s -mtriple=x86_64-unknown -mattr=+sse4.1 | FileCheck %s --check-prefix=ALL --check-prefix=SSE --check-prefix=SSE41
; RUN: llc < %s -mtriple=x86_64-unknown -mattr=+avx | FileCheck %s --check-prefix=ALL --check-prefix=AVX --check-prefix=AVX1
; RUN: llc < %s -mtriple=x86_64-unknown -mattr=+avx2 | FileCheck %s --check-prefix=ALL --check-prefix=AVX --check-prefix=AVX2
; RUN: llc < %s -mtriple=x86_64-unknown -mattr=+avx512f | FileCheck %s --check-prefix=ALL --check-prefix=AVX --check-prefix=AVX512F
;
; Combine tests involving SSE3/SSSE3 target shuffles (MOVDDUP, MOVSHDUP, MOVSLDUP, PSHUFB)

declare <16 x i8> @llvm.x86.ssse3.pshuf.b.128(<16 x i8>, <16 x i8>)

define <4 x float> @combine_pshufb_movddup(<4 x float> %a0) {
; SSE-LABEL: combine_pshufb_movddup:
; SSE:       # BB#0:
; SSE-NEXT:    pshufb {{.*#+}} xmm0 = xmm0[5,5,5,5,7,7,7,7,5,5,5,5,7,7,7,7]
; SSE-NEXT:    retq
;
; AVX-LABEL: combine_pshufb_movddup:
; AVX:       # BB#0:
; AVX-NEXT:    vpshufb {{.*#+}} xmm0 = xmm0[5,5,5,5,7,7,7,7,5,5,5,5,7,7,7,7]
; AVX-NEXT:    retq
  %1 = bitcast <4 x float> %a0 to <16 x i8>
  %2 = tail call <16 x i8> @llvm.x86.ssse3.pshuf.b.128(<16 x i8> %1, <16 x i8> <i8 5, i8 5, i8 5, i8 5, i8 7, i8 7, i8 7, i8 7, i8 1, i8 1, i8 1, i8 1, i8 3, i8 3, i8 3, i8 3>)
  %3 = bitcast <16 x i8> %2 to <4 x float>
  %4 = shufflevector <4 x float> %3, <4 x float> undef, <4 x i32> <i32 0, i32 1, i32 0, i32 1>
  ret <4 x float> %4
}

define <4 x float> @combine_pshufb_movshdup(<4 x float> %a0) {
; SSE-LABEL: combine_pshufb_movshdup:
; SSE:       # BB#0:
; SSE-NEXT:    pshufb {{.*#+}} xmm0 = xmm0[7,7,7,7,7,7,7,7,3,3,3,3,3,3,3,3]
; SSE-NEXT:    retq
;
; AVX-LABEL: combine_pshufb_movshdup:
; AVX:       # BB#0:
; AVX-NEXT:    vpshufb {{.*#+}} xmm0 = xmm0[7,7,7,7,7,7,7,7,3,3,3,3,3,3,3,3]
; AVX-NEXT:    retq
  %1 = bitcast <4 x float> %a0 to <16 x i8>
  %2 = tail call <16 x i8> @llvm.x86.ssse3.pshuf.b.128(<16 x i8> %1, <16 x i8> <i8 5, i8 5, i8 5, i8 5, i8 7, i8 7, i8 7, i8 7, i8 1, i8 1, i8 1, i8 1, i8 3, i8 3, i8 3, i8 3>)
  %3 = bitcast <16 x i8> %2 to <4 x float>
  %4 = shufflevector <4 x float> %3, <4 x float> undef, <4 x i32> <i32 1, i32 1, i32 3, i32 3>
  ret <4 x float> %4
}

define <4 x float> @combine_pshufb_movsldup(<4 x float> %a0) {
; SSE-LABEL: combine_pshufb_movsldup:
; SSE:       # BB#0:
; SSE-NEXT:    pshufb {{.*#+}} xmm0 = xmm0[5,5,5,5,5,5,5,5,1,1,1,1,1,1,1,1]
; SSE-NEXT:    retq
;
; AVX-LABEL: combine_pshufb_movsldup:
; AVX:       # BB#0:
; AVX-NEXT:    vpshufb {{.*#+}} xmm0 = xmm0[5,5,5,5,5,5,5,5,1,1,1,1,1,1,1,1]
; AVX-NEXT:    retq
  %1 = bitcast <4 x float> %a0 to <16 x i8>
  %2 = tail call <16 x i8> @llvm.x86.ssse3.pshuf.b.128(<16 x i8> %1, <16 x i8> <i8 5, i8 5, i8 5, i8 5, i8 7, i8 7, i8 7, i8 7, i8 1, i8 1, i8 1, i8 1, i8 3, i8 3, i8 3, i8 3>)
  %3 = bitcast <16 x i8> %2 to <4 x float>
  %4 = shufflevector <4 x float> %3, <4 x float> undef, <4 x i32> <i32 0, i32 0, i32 2, i32 2>
  ret <4 x float> %4
}
