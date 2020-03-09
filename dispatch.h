/* include file to initialize the CPU dispatch tables */

/* macros MRGEN_(VR) take as a Prime opcode number and set up the
   disp_(vr)mr V and R-mode dispatch tables.  The Prime opcode number
   for 4-bit opcode (in instruction bits 3-6) 1101 is 015, and Prime
   writes it as 01500 in older manuals. If the X bit is used as an
   opcode extension (only for opcode 01500 - non-indexable
   instructions), the opcode becomes 03500.  If bits 7-11 are 11000
   (for V-mode; bits 7-12 = 110000 in R-mode), the instruction is long
   form and has 2 extended opcode bits in bits 13-14.  The Prime
   equivalent would be 03500 - 03503.

   To summarize, the mem ref opcode index is a 7-bit value, 0-127:
   - bit 10 = bit 2 of instruction (X)
   - bits 11-14 = bits 3-6 of instruction
   - bits 15-16 = bits 13-14 of extended opcodes

   Instructions like JMP (opcode 01) that might be indexed will have
   2 entries in the dispatch table, 0 0001 00 and 1 0001 00, both
   pointing to the JMP emulation code.
*/

/* change V-mode MR instruction to opcode index */

#define VMRINSTIX(inst) ((((inst) >> 8) & 0x7C) | ((((inst) & 0x03E0) == 0x0300) ? (((inst) >> 2) & 3) : 0))

/* change R-mode MR instruction to opcode index 
   (mask is 1 bit longer for R-mode long instructions) */

#define RMRINSTIX(inst) ((((inst) >> 8) & 0x7C) | ((((inst) & 0x03F0) == 0x0300) ? (((inst) >> 2) & 3) : 0))

/* change S-mode MR instruction to opcode index (no long instructions) */

#define SMRINSTIX(inst) (((inst) >> 8) & 0x7C)

/* change "Prime manual" opcodes (35 03 for example) to dispatch index */

#define MRPRIMEIX(primeop) (((primeop) >> 4) | ((primeop) & 3))

/* set an entry in the R-mode memory reference opcode dispatch table */

#define MRGEN_R(opcode, name, target) \
  gv.disp_rmr[MRPRIMEIX(opcode)] = &&target; \
  /* printf("R-MR opcode %05o (%s), ix=%0d\n", opcode, name, MRPRIMEIX(opcode)); */ \
  if ((opcode & 01700) != 01500) { \
    gv.disp_rmr[MRPRIMEIX(opcode | 02000)] = &&target; \
    /* printf("R-MR opcode %05o (%s), ix=%0d\n", opcode | 02000, name, MRPRIMEIX(opcode | 02000)); */ \
  }

/* set an entry in the V-mode memory reference opcode dispatch table */

#define MRGEN_V(opcode, name, target) \
  gv.disp_vmr[MRPRIMEIX(opcode)] = &&target; \
  /* printf("V-MR opcode %05o (%s), ix=%0d\n", opcode, name, MRPRIMEIX(opcode)); */ \
  if ((opcode & 01700) != 01500) { \
    gv.disp_vmr[MRPRIMEIX(opcode | 02000)] = &&target; \
    /* printf("V-MR opcode %05o (%s), ix=%0d\n", opcode | 02000, name, MRPRIMEIX(opcode | 02000)); */ \
  }

/* initialize tables to "bad memory reference instruction" */

for (i=0; i < 128; i++) {
  gv.disp_rmr[i] = &&d_badmr;
  gv.disp_vmr[i] = &&d_badmr;
}

MRGEN_R(00100, "JMP", d_jmp);
MRGEN_V(00100, "JMP", d_jmp);

MRGEN_R(00101, "EAA", d_eaa);
MRGEN_V(00101, "EAL", d_eal);

MRGEN_R(00102, "XEC", d_xec);
MRGEN_V(00102, "XEC", d_xec);

MRGEN_R(00103, "ENTR", d_entr);
MRGEN_V(00103, "ENTR?", d_uii);

MRGEN_R(00200, "LDA/DLD", d_ldadld);
MRGEN_V(00200, "LDA", d_lda);

MRGEN_R(00201, "FLD", d_fld);
MRGEN_V(00201, "FLD", d_fld);

MRGEN_R(00202, "DFLD", d_dfld);
MRGEN_V(00202, "DFLD", d_dfld);

MRGEN_R(00203, "JEQ", d_jeq);
MRGEN_V(00203, "LDL", d_ldl);

MRGEN_R(00300, "ANA", d_ana);
MRGEN_V(00300, "ANA", d_ana);

MRGEN_R(00301, "STLR?", d_uii);
MRGEN_V(00301, "STLR", d_stlr);

MRGEN_R(00302, "ORA", d_ora);
MRGEN_V(00302, "ORA", d_ora);

MRGEN_R(00303, "JNE", d_jne);
MRGEN_V(00303, "ANL", d_anl);

MRGEN_R(00400, "STA/DST", d_stadst);
MRGEN_V(00400, "STA", d_sta);

MRGEN_R(00401, "FST", d_fst);
MRGEN_V(00401, "FST", d_fst);

MRGEN_R(00402, "DFST", d_dfst);
MRGEN_V(00402, "DFST", d_dfst);

MRGEN_R(00403, "JLE", d_jle);
MRGEN_V(00403, "STL", d_stl);

MRGEN_R(00500, "ERA", d_era);
MRGEN_V(00500, "ERA", d_era);

MRGEN_R(00501, "LDLR?", d_uii);
MRGEN_V(00501, "LDLR", d_ldlr);

MRGEN_R(00502, "QFxx", d_qfxxuii);
MRGEN_V(00502, "QFxx", d_qfxxuii);

MRGEN_R(00503, "JGT", d_jgt);
MRGEN_V(00503, "ERL", d_erl);

MRGEN_R(00600, "ADD/DAD", d_adddad);
MRGEN_V(00600, "ADD", d_add);

MRGEN_R(00601, "FAD", d_fad);
MRGEN_V(00601, "FAD", d_fad);

MRGEN_R(00602, "DFAD", d_dfad);
MRGEN_V(00602, "DFAD", d_dfad);

MRGEN_R(00603, "JLT", d_jlt);
MRGEN_V(00603, "ADL", d_adl);

MRGEN_R(00700, "SUB/DSB", d_subdsb);
MRGEN_V(00700, "SUB", d_sub);

MRGEN_R(00701, "FSB", d_fsb);
MRGEN_V(00701, "FSB", d_fsb);

MRGEN_R(00702, "DFSB", d_dfsb);
MRGEN_V(00702, "DFSB", d_dfsb);

MRGEN_R(00703, "JGE", d_jge);
MRGEN_V(00703, "SBL", d_sbl);

MRGEN_R(01000, "JST", d_jst);
MRGEN_V(01000, "JST", d_jst);

MRGEN_R(01002, "CREP", d_crep);
MRGEN_V(01002, "PCL", d_pcl);

MRGEN_R(01100, "CAS", d_cas);
MRGEN_V(01100, "CAS", d_cas);

MRGEN_R(01101, "FCS", d_fcs);
MRGEN_V(01101, "FCS", d_fcs);

MRGEN_R(01102, "DFCS", d_dfcs);
MRGEN_V(01102, "DFCS", d_dfcs);

MRGEN_R(01103, "CLS?", d_uii);
MRGEN_V(01103, "CLS", d_cls);

MRGEN_R(01200, "IRS", d_irs);
MRGEN_V(01200, "IRS", d_irs);

MRGEN_R(01202, "EAXB?", d_uii);
MRGEN_V(01202, "EAXB", d_eaxb);

MRGEN_R(01300, "IMA", d_ima);
MRGEN_V(01300, "IMA", d_ima);

MRGEN_R(01302, "EALB?", d_uii);
MRGEN_V(01302, "EALB", d_ealb);

MRGEN_R(01400, "JSY?", d_uii);
MRGEN_V(01400, "JSY", d_jsy);

MRGEN_R(01401, "EIO?", d_uii);
MRGEN_V(01401, "EIO", d_eio);

MRGEN_R(01402, "JSXB?", d_uii);
MRGEN_V(01402, "JSXB", d_jsxb);

MRGEN_R(01500, "STX", d_stx);
MRGEN_V(01500, "STX", d_stx);

MRGEN_R(01501, "FLX", d_flx);
MRGEN_V(01501, "FLX", d_flx);

MRGEN_R(01502, "JDX", d_jdx);
MRGEN_V(01502, "DFLX", d_dflx);

MRGEN_R(01503, "JIX", d_jix);
MRGEN_V(01503, "QFLX", d_qflx);

MRGEN_R(01600, "MPY", d_mpy_r);
MRGEN_V(01600, "MPY", d_mpy);

MRGEN_R(01601, "FMP", d_fmp);
MRGEN_V(01601, "FMP", d_fmp);

MRGEN_R(01602, "DFMP", d_dfmp);
MRGEN_V(01602, "DFMP", d_dfmp);

MRGEN_R(01603, "MPL?", d_uii);
MRGEN_V(01603, "MPL", d_mpl);

MRGEN_R(01700, "DIV", d_div);
MRGEN_V(01700, "DIV", d_div);

MRGEN_R(01701, "FDV", d_fdv);
MRGEN_V(01701, "FDV", d_fdv);

MRGEN_R(01702, "DFDV", d_dfdv);
MRGEN_V(01702, "DFDV", d_dfdv);

MRGEN_R(01703, "DVL?", d_uii);
MRGEN_V(01703, "DVL", d_dvl);

MRGEN_R(03500, "LDX", d_ldx);
MRGEN_V(03500, "LDX", d_ldx);

MRGEN_R(03501, "LDY?", d_uii);
MRGEN_V(03501, "LDY", d_ldy);

MRGEN_R(03502, "STY?", d_uii);
MRGEN_V(03502, "STY", d_sty);

MRGEN_R(03503, "JSX", d_jsx);
MRGEN_V(03503, "JSX", d_jsx);


#define GENIX(inst) ((inst>>4) & 06000) | (inst & 01777)

#define DIGEN(opcode, name, target) \
  disp_gen[GENIX(opcode)] = &&target; \
  //printf("Opcode %06o (%s), ix=%0o\n", opcode, name, GENIX(opcode))

/* initialize entire table to jump to bad generic label */

for (i=0; i < 010000; i++) {
  disp_gen[i] = &&d_badgen;
}

/* initialize class 0 generics (first 2 bits are zero) */

DIGEN(000000, "HLT", d_hlt);
DIGEN(000201, "IAB", d_iab);
DIGEN(001314, "CGT", d_cgt);
DIGEN(000115, "PIDA", d_pida);
DIGEN(000305, "PIDL", d_pidl);
DIGEN(000015, "PIMA", d_pima);
DIGEN(000301, "PIML", d_piml);
DIGEN(001302, "LDC 0", d_ldc0);
DIGEN(001312, "LDC 1", d_ldc1);
DIGEN(001322, "STC 0", d_stc0);
DIGEN(001332, "STC 1", d_stc1);
DIGEN(001300, "EAFA 0", d_eafa0);
DIGEN(001310, "EAFA 1", d_eafa1);
DIGEN(001301, "ALFA 0", d_alfa0);
DIGEN(001311, "ALFA 1", d_alfa1);
DIGEN(001303, "LFLI 0", d_lfli0);
DIGEN(001313, "LFLI 1", d_lfli1);
DIGEN(001320, "STFA 0", d_stfa0);
DIGEN(001330, "STFA 1", d_stfa1);
DIGEN(001321, "TLFL 0", d_tlfl0);
DIGEN(001331, "TLFL 1", d_tlfl1);
DIGEN(001323, "TFLL 0", d_tfll0);
DIGEN(001333, "TFLL 1", d_tfll1);
DIGEN(000611, "PRTN", d_prtn);
DIGEN(001005, "TKA", d_tka);
DIGEN(001015, "TAK", d_tak);
DIGEN(000001, "NOP 1", d_nop);
DIGEN(000715, "RSAV", d_rsav);
DIGEN(000717, "RRST", d_rrst);
DIGEN(000400, "ENBM", d_enb);
DIGEN(000401, "ENBL", d_enb);
DIGEN(000402, "ENBP", d_enb);
DIGEN(001000, "INHM", d_inh);
DIGEN(001001, "INHL", d_inh);
DIGEN(001002, "INHP", d_inh);
DIGEN(001200, "STAC", d_stac);
DIGEN(001204, "STLC", d_stlc);
DIGEN(000605, "ARGT", d_argt);
DIGEN(000705, "CALF", d_calf);
DIGEN(001114, "ZMV", d_zmv);
DIGEN(001115, "ZMVD", d_zmvd);
DIGEN(001116, "ZFIL", d_zfil);
DIGEN(001117, "ZCM", d_zcm);
DIGEN(001110, "ZTRN", d_ztrn);
DIGEN(001111, "ZED", d_zed);
DIGEN(001112, "XED", d_xed);
DIGEN(001100, "XAD", d_xuii);
DIGEN(001101, "XMV", d_xuii);
DIGEN(001102, "XCM", d_xuii);
DIGEN(001104, "XMP", d_xuii);
DIGEN(001107, "XDV", d_xuii);
DIGEN(001145, "XBTD", d_xuii);
DIGEN(001146, "XDTB", d_xuii);
DIGEN(000510, "STTM", d_sttm);
DIGEN(000511, "RTS", d_rts);
DIGEN(000315, "WAIT", d_wait);
DIGEN(001210, "NFYE", d_nfy);
DIGEN(001211, "NFYB", d_nfy);
DIGEN(001214, "INEN", d_nfy);
DIGEN(001215, "INBN", d_nfy);
DIGEN(001216, "INEC", d_nfy);
DIGEN(001217, "INBC", d_nfy);
DIGEN(001315, "STEX", d_stex);
DIGEN(000044, "LIOT", d_liot);
DIGEN(000064, "PTLB", d_ptlb);
DIGEN(000615, "ITLB", d_itlb);
DIGEN(000711, "LPSW", d_lpsw);
DIGEN(000024, "STPM", d_stpm);
DIGEN(001700, "DBG0", d_dbgill);
DIGEN(001701, "DBG1", d_dbgill);
DIGEN(001702, "PBUG", d_pbug);
DIGEN(000601, "IRTN", d_irtn);
DIGEN(000603, "IRTC", d_irtc);
DIGEN(000411, "CAI", d_cai);
DIGEN(000005, "SGL", d_sgl);
DIGEN(000011, "E16S", d_e16s);
DIGEN(000013, "E32S", d_e32s);
DIGEN(001013, "E32R", d_e32r);
DIGEN(001011, "E64R", d_e64r);
DIGEN(000010, "E64V", d_e64v);
DIGEN(001010, "E32I", d_e32i);
DIGEN(000505, "SVC", d_svc);
DIGEN(000111, "CEA", d_cea);
DIGEN(000205, "PIM", d_pim);  /* R-mode */
DIGEN(000211, "PID", d_pid);  /* R-mode */
DIGEN(000007, "DBL", d_dbl);  /* R-mode */
DIGEN(000041, "SCA", d_sca);  /* R-mode */
DIGEN(000043, "INKr", d_inkr);  /* R-mode */
DIGEN(000405, "OTKr", d_otkr);  /* R-mode */
DIGEN(000415, "ESIM", d_esim);
DIGEN(000417, "EVIM", d_evim);
DIGEN(000101, "NRM", d_nrm);  /* R-mode */
DIGEN(000105, "RTN", d_rtn);  /* R-mode */
DIGEN(000003, "SYNC", d_sync);
DIGEN(000503, "EMCM", d_emcm);
DIGEN(000501, "LMCM", d_lmcm);
DIGEN(000021, "RMC", d_rmc);
DIGEN(000311, "VIRY", d_viry);
DIGEN(001113, "XVFY", d_xvfy);
DIGEN(001304, "MDEI", d_mdxx);
DIGEN(001305, "MDII", d_mdxx);
DIGEN(001306, "MDRS", d_mdxx);
DIGEN(001307, "MDWC", d_mdxx);
DIGEN(001324, "MDIW", d_mdxx);

DIGEN(000217, "EPMJ", d_uii);
DIGEN(000215, "LPMJ", d_uii);
DIGEN(000237, "EPMX", d_uii);
DIGEN(000235, "LPMX", d_uii);
DIGEN(000703, "EVMJ", d_uii);
DIGEN(000701, "ERMJ", d_uii);
DIGEN(000723, "EVMX", d_uii);
DIGEN(000721, "ERMX", d_uii);
DIGEN(000515, "OSI", d_uii);

DIGEN(000617, "LPID", d_lpid);

/* initialize entire class 1 generics (shift group) to "badshift",
   then initialize each individual shift instruction */

for (i = 02000; i < 04000; i++) {
  disp_gen[i] = &&d_badshift;
}

for (i = 02000; i < 02100; i++) {
  disp_gen[i] = &&d_lrl;
}

for (i = 02100; i < 02200; i++) {
  disp_gen[i] = &&d_lrs;
}

for (i = 02200; i < 02300; i++) {
  disp_gen[i] = &&d_lrr;
}

for (i = 02300; i < 02400; i++) {
  disp_gen[i] = &&d_300shift;
}

for (i = 02400; i < 02500; i++) {
  disp_gen[i] = &&d_arl;
}

for (i = 02500; i < 02600; i++) {
  disp_gen[i] = &&d_ars;
}

for (i = 02600; i < 02700; i++) {
  disp_gen[i] = &&d_arr;
}

for (i = 03000; i < 03100; i++) {
  disp_gen[i] = &&d_lll;
}

for (i = 03100; i < 03200; i++) {
  disp_gen[i] = &&d_lls;
}

for (i = 03200; i < 03300; i++) {
  disp_gen[i] = &&d_llr;
}

for (i = 03400; i < 03500; i++) {
  disp_gen[i] = &&d_all;
}

for (i = 03500; i < 03600; i++) {
  disp_gen[i] = &&d_als;
}

for (i = 03600; i < 03700; i++) {
  disp_gen[i] = &&d_alr;
}

/* initialize class 2 generics (skip group) */

#if 0
for (i = 04000; i < 06000; i++) {
  disp_gen[i] = &&d_gen2;
}
#endif

DIGEN(0101000, "NOP-SKP", d_nopskp);
DIGEN(0100000, "SKP", d_skp);
DIGEN(0101400, "SMI/SLT", d_smi);
DIGEN(0100400, "SPL/SGE", d_spl);
DIGEN(0101100, "SLN", d_sln);
DIGEN(0100100, "SLZ", d_slz);
DIGEN(0101040, "SNZ/SNE", d_snz);
DIGEN(0100040, "SZE/SEQ", d_sze);
DIGEN(0101220, "SLE", d_sle);
DIGEN(0100220, "SGT", d_sgt);
DIGEN(0101001, "SSC", d_ssc);
DIGEN(0100001, "SRC", d_src);
DIGEN(0100260, "SAR 1", d_sar);
DIGEN(0100261, "SAR 2", d_sar);
DIGEN(0100262, "SAR 3", d_sar);
DIGEN(0100263, "SAR 4", d_sar);
DIGEN(0100264, "SAR 5", d_sar);
DIGEN(0100265, "SAR 6", d_sar);
DIGEN(0100266, "SAR 7", d_sar);
DIGEN(0100267, "SAR 8", d_sar);
DIGEN(0100270, "SAR 9", d_sar);
DIGEN(0100271, "SAR 10", d_sar);
DIGEN(0100272, "SAR 11", d_sar);
DIGEN(0100273, "SAR 12", d_sar);
DIGEN(0100274, "SAR 13", d_sar);
DIGEN(0100275, "SAR 14", d_sar);
DIGEN(0100276, "SAR 15", d_sar);
DIGEN(0100277, "SAR 16", d_sar);
DIGEN(0101260, "SAS 1", d_sas);
DIGEN(0101261, "SAS 2", d_sas);
DIGEN(0101262, "SAS 3", d_sas);
DIGEN(0101263, "SAS 4", d_sas);
DIGEN(0101264, "SAS 5", d_sas);
DIGEN(0101265, "SAS 6", d_sas);
DIGEN(0101266, "SAS 7", d_sas);
DIGEN(0101267, "SAS 8", d_sas);
DIGEN(0101270, "SAS 9", d_sas);
DIGEN(0101271, "SAS 10", d_sas);
DIGEN(0101272, "SAS 11", d_sas);
DIGEN(0101273, "SAS 12", d_sas);
DIGEN(0101274, "SAS 13", d_sas);
DIGEN(0101275, "SAS 14", d_sas);
DIGEN(0101276, "SAS 15", d_sas);
DIGEN(0101277, "SAS 16", d_sas);
DIGEN(0100240, "SNR 1", d_snr);
DIGEN(0100241, "SNR 2", d_snr);
DIGEN(0100242, "SNR 3", d_snr);
DIGEN(0100243, "SNR 4", d_snr);
DIGEN(0100244, "SNR 5", d_snr);
DIGEN(0100245, "SNR 6", d_snr);
DIGEN(0100246, "SNR 7", d_snr);
DIGEN(0100247, "SNR 8", d_snr);
DIGEN(0100250, "SNR 9", d_snr);
DIGEN(0100251, "SNR 10", d_snr);
DIGEN(0100252, "SNR 11", d_snr);
DIGEN(0100253, "SNR 12", d_snr);
DIGEN(0100254, "SNR 13", d_snr);
DIGEN(0100255, "SNR 14", d_snr);
DIGEN(0100256, "SNR 15", d_snr);
DIGEN(0100257, "SNR 16", d_snr);
DIGEN(0101240, "SNS 1", d_sns);
DIGEN(0101241, "SNS 2", d_sns);
DIGEN(0101242, "SNS 3", d_sns);
DIGEN(0101243, "SNS 4", d_sns);
DIGEN(0101244, "SNS 5", d_sns);
DIGEN(0101245, "SNS 6", d_sns);
DIGEN(0101246, "SNS 7", d_sns);
DIGEN(0101247, "SNS 8", d_sns);
DIGEN(0101250, "SNS 9", d_sns);
DIGEN(0101251, "SNS 10", d_sns);
DIGEN(0101252, "SNS 11", d_sns);
DIGEN(0101253, "SNS 12", d_sns);
DIGEN(0101254, "SNS 13", d_sns);
DIGEN(0101255, "SNS 14", d_sns);
DIGEN(0101256, "SNS 15", d_sns);
DIGEN(0101257, "SNS 16", d_sns);
DIGEN(0100200, "SMCR", d_smcr);
DIGEN(0101200, "SMCS", d_smcs);
DIGEN(0101020, "SS1", d_ssx);
DIGEN(0100020, "SR1", d_srx);
DIGEN(0101010, "SS2", d_ssx);
DIGEN(0100010, "SR2", d_srx);
DIGEN(0101004, "SS3", d_ssx);
DIGEN(0100004, "SR3", d_srx);
DIGEN(0101002, "SS4", d_ssx);
DIGEN(0100002, "SR4", d_srx);
DIGEN(0101036, "SSS", d_ssx);
DIGEN(0100036, "SSR", d_srx);

/* initialize class 3 generics */

DIGEN(0141604, "BCLT", d_bclt);
DIGEN(0141600, "BCLE", d_bcle);
DIGEN(0141602, "BCEQ", d_bceq);
DIGEN(0141603, "BCNE", d_bcne);
DIGEN(0141605, "BCGE", d_bcge);
DIGEN(0141601, "BCGT", d_bcgt);
DIGEN(0141705, "BCR", d_bcr);
DIGEN(0141704, "BCS", d_bcs);
DIGEN(0141707, "BLR", d_blr);  /* also BMLT */
DIGEN(0141706, "BLS", d_bls);
DIGEN(0140614, "BLT", d_blt);
DIGEN(0140610, "BLE", d_ble);
DIGEN(0140612, "BEQ", d_beq);
DIGEN(0140613, "BNE", d_bne);
DIGEN(0140615, "BGE", d_bge);
DIGEN(0140611, "BGT", d_bgt);
DIGEN(0140700, "BLLE", d_blle);
DIGEN(0140702, "BLEQ", d_bleq);
DIGEN(0140703, "BLNE", d_blne);
DIGEN(0140701, "BLGT", d_blgt);
DIGEN(0141614, "BFLT", d_bflt);
DIGEN(0141610, "BFLE", d_bfle);
DIGEN(0141612, "BFEQ", d_bfeq);
DIGEN(0141613, "BFNE", d_bfne);
DIGEN(0141615, "BFGE", d_bfge);
DIGEN(0141611, "BFGT", d_bfgt);
DIGEN(0141334, "BIX", d_bix);
DIGEN(0141324, "BIY", d_biy);
DIGEN(0140724, "BDY", d_bdy);
DIGEN(0140734, "BDX", d_bdx);
DIGEN(0141206, "A1A", d_a1a);  /* aka AOA */
DIGEN(0140304, "A2A", d_a2a);
DIGEN(0141216, "ACA", d_aca);
DIGEN(0140110, "S1A", d_s1a);
DIGEN(0140310, "S2A", d_s2a);
DIGEN(0141050, "CAL", d_cal);
DIGEN(0141044, "CAR", d_car);
DIGEN(0140040, "CRA", d_cra);
DIGEN(0140014, "CRB300", d_crb300);
DIGEN(0140015, "CRB", d_crb);
DIGEN(0140016, "FDBL", d_fdbl);
DIGEN(0140010, "CRL", d_crl);
DIGEN(0140214, "CAZ", d_caz);
DIGEN(0140114, "IRX", d_irx);
DIGEN(0140210, "DRX", d_drx);
DIGEN(0141240, "ICR", d_icr);
DIGEN(0141140, "ICL", d_icl);
DIGEN(0141340, "ICA", d_ica);
DIGEN(0140417, "LT", d_lt);
DIGEN(0140416, "LF", d_lf);
DIGEN(0140314, "TAB", d_tab);
DIGEN(0140504, "TAX", d_tax);
DIGEN(0140505, "TAY", d_tay);
DIGEN(0140604, "TBA", d_tba);
DIGEN(0141034, "TXA", d_txa);
DIGEN(0141124, "TYA", d_tya);
DIGEN(0140104, "XCA", d_xca);
DIGEN(0140204, "XCB", d_xcb);
DIGEN(0140407, "TCA", d_tca);
DIGEN(0141210, "TCL", d_tcl);
DIGEN(0140600, "SCB", d_scb);
DIGEN(0140200, "RCB", d_rcb);
DIGEN(0140024, "CHS", d_chs);
DIGEN(0140500, "SSM", d_ssm);
DIGEN(0140100, "SSP", d_ssp);
DIGEN(0140401, "CMA", d_cma);
DIGEN(0140320, "CSA", d_csa);
DIGEN(0141500, "LCLT", d_lclt);
DIGEN(0141501, "LCLE", d_lcle);
DIGEN(0141503, "LCEQ", d_lceq);
DIGEN(0141502, "LCNE", d_lcne);
DIGEN(0141504, "LCGE", d_lcge);
DIGEN(0141505, "LCGT", d_lcgt);
DIGEN(0140410, "LLT", d_llt);
DIGEN(0140411, "LLE", d_lle);
DIGEN(0140412, "LNE", d_lne);
DIGEN(0140413, "LEQ", d_leq);
DIGEN(0140414, "LGE", d_lge);
DIGEN(0140415, "LGT", d_lgt);
DIGEN(0141511, "LLLE", d_llle);
DIGEN(0141513, "LLEQ", d_lleq);
DIGEN(0141512, "LLNE", d_llne);
DIGEN(0141515, "LLGT", d_llgt);
DIGEN(0141110, "LFLT", d_lflt);
DIGEN(0141111, "LFLE", d_lfle);
DIGEN(0141113, "LFEQ", d_lfeq);
DIGEN(0141112, "LFNE", d_lfne);
DIGEN(0141114, "LFGE", d_lfge);
DIGEN(0141115, "LFGT", d_lfgt);
DIGEN(0140550, "FLOT", d_flot);
DIGEN(0140534, "FRN", d_frn);
DIGEN(0140574, "DFCM", d_dfcm);
DIGEN(0141000, "ADLL", d_adll);
DIGEN(0140530, "FCMv", d_fcmv);
DIGEN(0140510, "FSZE", d_fsze);
DIGEN(0140511, "FSNZ", d_fsnz);
DIGEN(0140512, "FSMI", d_fsmi);
DIGEN(0140513, "FSPL", d_fspl);
DIGEN(0140514, "FSLE", d_fsle);
DIGEN(0140515, "FSGT", d_fsgt);
DIGEN(0140554, "INT", d_int);
DIGEN(0140531, "INTA", d_inta);
DIGEN(0140532, "FLTA", d_flta);
DIGEN(0140533, "INTL", d_intl);
DIGEN(0140535, "FLTL", d_fltl);
DIGEN(0141711, "BMLE", d_bmle);
/* DIGEN(0141606, "BMGE", d_bmge); */  /* replaced by BLS */
DIGEN(0141710, "BMGT", d_bmgt);
DIGEN(0141404, "CRE", d_cre);
DIGEN(0141410, "CRLE", d_crle);
DIGEN(0141414, "ILE", d_ile);
DIGEN(0140570, "QFCM", d_quii);
DIGEN(0140571, "DRNM", d_quii);
DIGEN(0140572, "QINQ", d_quii);
DIGEN(0140573, "QIQR", d_quii);
DIGEN(0141714, "RTQ", d_rtq);
DIGEN(0141715, "RBQ", d_rbq);
DIGEN(0141716, "ABQ", d_abq);
DIGEN(0141717, "ATQ", d_atq);
DIGEN(0141757, "TSTQ", d_tstq);
DIGEN(0141700, "DIAGILL", d_diagill);
