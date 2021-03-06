//===- README_P9.txt - Notes for improving Power9 code gen ----------------===//

TODO: Instructions Need Implement Instrinstics or Map to LLVM IR

Altivec:

VSX:

- QP Compare Ordered/Unordered: xscmpoqp xscmpuqp
  . ref: XSCMPUDP
      def XSCMPUDP : XX3Form_1<60, 35,
                               (outs crrc:$crD), (ins vsfrc:$XA, vsfrc:$XB),
                               "xscmpudp $crD, $XA, $XB", IIC_FPCompare, []>;

  . No SDAG, intrinsic, builtin are required??
    Or llvm fcmp order/unorder compare??

- DP/QP Compare Exponents: xscmpexpdp xscmpexpqp
  . No SDAG, intrinsic, builtin are required?

- DP Compare ==, >=, >, !=: xscmpeqdp xscmpgedp xscmpgtdp xscmpnedp
  . I checked existing instruction "XSCMPUDP". They are different in target
    register. "XSCMPUDP" write to CR field, xscmp*dp write to VSX register

  . Use instrinsic:
    (set i128:$XT, (int_ppc_vsx_xscmpeqdp f64:$XA, f64:$XB))
    (set i128:$XT, (int_ppc_vsx_xscmpgedp f64:$XA, f64:$XB))
    (set i128:$XT, (int_ppc_vsx_xscmpgtdp f64:$XA, f64:$XB))
    (set i128:$XT, (int_ppc_vsx_xscmpnedp f64:$XA, f64:$XB))

- Vector Compare Not Equal: xvcmpnedp xvcmpnedp. xvcmpnesp xvcmpnesp.
  . Similar to xvcmpeqdp:
      defm XVCMPEQDP : XX3Form_Rcr<60, 99,
                                 "xvcmpeqdp", "$XT, $XA, $XB", IIC_VecFPCompare,
                                 int_ppc_vsx_xvcmpeqdp, v2i64, v2f64>;

  . So we should use "XX3Form_Rcr" to implement instrinsic

- Convert DP -> QP: xscvdpqp
  . Similar to XSCVDPSP:
      def XSCVDPSP : XX2Form<60, 265,
                          (outs vsfrc:$XT), (ins vsfrc:$XB),
                          "xscvdpsp $XT, $XB", IIC_VecFP, []>;
  . So, No SDAG, intrinsic, builtin are required??

- Round & Convert QP -> DP (dword[1] is set to zero): xscvqpdp xscvqpdpo
  . Similar to XSCVDPSP
  . No SDAG, intrinsic, builtin are required??

- Truncate & Convert QP -> (Un)Signed (D)Word (dword[1] is set to zero):
  xscvqpsdz xscvqpswz xscvqpudz xscvqpuwz
  . According to PowerISA_V3.0, these are similar to "XSCVDPSXDS", "XSCVDPSXWS",
    "XSCVDPUXDS", "XSCVDPUXWS"

  . DAG patterns:
    (set f128:$XT, (PPCfctidz f128:$XB))    // xscvqpsdz
    (set f128:$XT, (PPCfctiwz f128:$XB))    // xscvqpswz
    (set f128:$XT, (PPCfctiduz f128:$XB))   // xscvqpudz
    (set f128:$XT, (PPCfctiwuz f128:$XB))   // xscvqpuwz

- Convert (Un)Signed DWord -> QP: xscvsdqp xscvudqp
  . Similar to XSCVSXDSP
  . (set f128:$XT, (PPCfcfids f64:$XB))     // xscvsdqp
    (set f128:$XT, (PPCfcfidus f64:$XB))    // xscvudqp

- (Round &) Convert DP <-> HP: xscvdphp xscvhpdp
  . Similar to XSCVDPSP
  . No SDAG, intrinsic, builtin are required??

- Vector HP -> SP: xvcvhpsp xvcvsphp
  . Similar to XVCVDPSP:
      def XVCVDPSP : XX2Form<60, 393,
                          (outs vsrc:$XT), (ins vsrc:$XB),
                          "xvcvdpsp $XT, $XB", IIC_VecFP, []>;
  . No SDAG, intrinsic, builtin are required??

- Round to Quad-Precision Integer: xsrqpi xsrqpix
  . These are combination of "XSRDPI", "XSRDPIC", "XSRDPIM", .., because you
    need to assign rounding mode in instruction
  . Provide builtin?
    (set f128:$vT, (int_ppc_vsx_xsrqpi f128:$vB))
    (set f128:$vT, (int_ppc_vsx_xsrqpix f128:$vB))

- Round Quad-Precision to Double-Extended Precision (fp80): xsrqpxp
  . Provide builtin?
    (set f128:$vT, (int_ppc_vsx_xsrqpxp f128:$vB))

