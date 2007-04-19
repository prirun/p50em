/* this version is derived from the flowchart in the preliminary P400
   release notes */

inline ea_t ea64v (ea_t earp, unsigned short inst, short x, unsigned short *opcode) {

  ea_t ea;                                       /* full seg/word va */
  unsigned short ea_s;                           /* eff address segno */
  unsigned short ea_r;                           /* eff address ring */
  unsigned short ea_w;                           /* eff address wordno */
  unsigned short br;
  unsigned short live;                           /* max live register addr */
  unsigned short i;
  unsigned short y;
  unsigned short xok;
  unsigned short a;
  unsigned short ixy;
  unsigned short m;
  unsigned short rph,rpl;

  i = inst & 0100000;           /* indirect is bit 1 (left/MS bit) */

  /* rph/rpl (and earp) are usually = RPH/RPL in the register file,
     except for the case of an XEC instruction; in that case, these
     will point to 1 after the instruction being executed */

  rph = earp >> 16;
  rpl = earp & 0xFFFF;
  //TRACE(T_EAV, " inst=%o, rph=%o, rpl=%o\n", inst, rph, rpl);

  if (crs[MODALS] & 4)                           /* segmentation enabled? */
    live = 010;                                  /* yes, limit register traps */
  else
    live = 040;

  ea_s = rph;
  ea_w = rpl;
  if (inst & 001000)                             /* sector bit 7 set? */
    if ((inst & 0740) != 0400) {                 /* PC relative? */
      ea_w = rpl + (((short) (inst << 7)) >> 7);   /* yes, sign extend D */
      TRACE(T_EAV, " PC relative, P=%o, new ea_w=%o\n", rpl, ea_w);
    }
    else 
      goto labB;                                 /* special cases */
  else if (i) {
    ea_w = (inst & 0777);                        /* sector 0 */
    TRACE(T_EAV, " Sector 0, new ea_w=%o\n", ea_w);
    if (ea_w < 0100 && x) {                      /* preindex by X */
      TRACE(T_EAV, " Preindex, ea_w=%o, X='%o/%d\n", ea_w, crs[X], *(short *)(crs+X));
      ea_w += crs[X];
      TRACE(T_EAV, " Preindex, new ea_w=%o\n", ea_w);
      x = 0;
    }
  } else 
    goto labA;

  if (i) {
    if (ea_w < live) {
      TRACE(T_EAV, " Indirect through live register '%o\n", ea_w);
      ea_w = get16(0x80000000 | ea_w);
    } else {
      TRACE(T_EAV, " Indirect, ea_s=%o, ea_w=%o\n", ea_s, ea_w);
      ea_w = get16(MAKEVA(ea_s, ea_w));
    }
    TRACE(T_EAV, " After indirect, new ea_w=%o\n", ea_w);
  }
  if (x) {
    TRACE(T_EAV, " Postindex, old ea_w=%o, X='%o/%d\n", ea_w, crs[X], *(short *)(crs+X));
    ea_w += crs[X];
    TRACE(T_EAV, " Postindex, new ea_w=%o\n", ea_w);
  }

  if (ea_w < live) {
    TRACE(T_EAV, " Live register '%o\n", ea_w);
    return 0x80000000 | ea_w;
  }
  return MAKEVA(ea_s, ea_w);


labA:
  ea_w = (inst & 0777);
  if (x) {
    TRACE(T_EAV, " Postindex, old ea_w=%o, X='%o/%d\n", ea_w, crs[X], *(short *)(crs+X));
    ea_w += crs[X];
    TRACE(T_EAV, " Postindex, new ea_w=%o\n", ea_w);
  }
  if ((inst & 0777) >= 0400) {
    ea_s = crs[LBH] | (ea_s & RINGMASK16);
    ea_w += crs[LBL];
    TRACE(T_EAV, " Short LB relative, LB=%o/%o\n", crs[LBH], crs[LBL]);
    return MAKEVA(ea_s, ea_w);
  }
  if (ea_w < live) {
    TRACE(T_EAV, " Live register '%o\n", ea_w);
    return 0x80000000 | ea_w;
  }
  ea_s = crs[SBH] | (ea_s & RINGMASK16);
  ea_w += crs[SBL];
  TRACE(T_EAV, " Short SB relative, SB=%o/%o\n", crs[SBH], crs[SBL]);
  return MAKEVA(ea_s, ea_w);
  

  /* here for long, 2-word, V-mode memory reference */

labB:
  a = iget16(RP);
  RPL++;
  TRACE(T_EAV, " 2-word format, a=%o\n", a);
  y = (inst & 020);
  ixy = ((i != 0)<<2) | ((x != 0)<<1) | (y != 0);
  xok = ((*opcode & 01700) != 01500);        /* true if indexing is okay */

  *opcode = *opcode | ((inst >> 2) & 3);         /* opcode extension */
  br = (inst & 3);
  TRACE(T_EAV, " new opcode=%5#0o, y=%d, br=%d, ixy=%d, xok=%d\n", *opcode, (y != 0), br, ixy, xok);

  ea_s = crs[PBH+br*2] | (ea_s & RINGMASK16);
  ea_w = crs[PBL+br*2] + a;

  if (xok)
    if (ixy == 1 || ixy == 4)
      ea_w += crs[Y];
    else if (ixy == 2 || ixy == 6)
      ea_w += crs[X];

  if (ixy >= 3) {
    ea = MAKEVA(ea_s, ea_w);
    TRACE(T_EAV, " Long indirect, ea=%o/%o, ea_s=%o, ea_w=%o\n", ea>>16, ea&0xFFFF, ea_s, ea_w);
    m = get16(ea);
    if (m & 0x8000)
      fault(POINTERFAULT, m, ea);
    ea_s = m | (ea_s & RINGMASK16);
    ea_w = get16(INCVA(ea,1));
#if 0
    if (ea_s & EXTMASK16)
      warn("em: extension bit set in ea64v");
#endif
    TRACE(T_EAV, " After indirect, ea_s=%o, ea_w=%o\n", ea_s, ea_w);
  }
  if (xok)
    if (ixy == 5)
      ea_w += crs[Y];
    else if (ixy == 7)
      ea_w += crs[X];
  return MAKEVA(ea_s, ea_w);
}
