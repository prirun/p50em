#define IMM_EA 0x80000000

inline ea_t ea32i (ea_t earp, unsigned short inst, unsigned long *immu32, unsigned long long *immu64) {

#ifdef OSX
  int tm, sr, br, ring;
  unsigned short d;
  int temp32;
  ea_t ea, ip;

  *immu32 = 0xAAAAAAAA;
  *immu64 = 0xAAAAAAAAAAAAAAAALL;

  tm = (inst >> 5) & 3;
  sr = (inst >> 2) & 7;
  br = inst & 3;
  ring = RP & RINGMASK32;
  TRACE(T_EAI, " tm=%d, sr=%d, dr=%d, br=%d\n", tm, sr, (inst >> 7) & 7, br);

  switch (tm) {
  case 0:
    switch (br) {
    case 0:                                   /* reg-reg */
      *immu32 = crsl[sr];
      return IMM_EA;

    case 1:
      d = iget16(RP);
      RPL++;
      if (sr == 0)                            /* imm type 1 */
	*immu32 = d << 16;
      else                                    /* imm type 2 */
	*(int *)immu32 = *(short *)&d;
      return IMM_EA;

    case 2:
      switch (sr) {
      case 0:                                 /* imm type 3 */
	d = iget16(RP);
	RPL++;
	*immu64 = (((long long)(d & 0xFF00)) << 48) | (d & 0xFF);
	return IMM_EA;
      case 1:                                 /* FAC0 source */
	*immu64 = *(unsigned long long *)(crsl+FAC0);
	return IMM_EA;
      case 3:                                 /* FAC1 source */
	*immu64 = *(unsigned long long *)(crsl+FAC1);
	return IMM_EA;
      case 2:
      case 4:
      case 5:
      case 6:
      case 7:
	fault(UIIFAULT, RPL, RP);
	fatal("ea32i: return from UII fault!");
      default:
	fatal("ea32i: sr < 0 or > 7?");
      }
      fatal("ea32i: case tm=0 br=2 fall-through");

    case 3:                                   /* GR relative */
      d = iget16(RP);
      RPL++;
      ea = (crsl[sr] & 0xFFFF0000) | ((crsl[sr] + d) & 0xFFFF);
      if (ea & 0x80000000)
	fault(POINTERFAULT, ea>>16, ea);
      return ea | ring;

    default:
      fatal("ea32i: tm=0, br < 0 or > 3?");
    }
    fatal("ea32i: tm=0 fall-through");

  case 1:  /* TM=1: Direct and Indexed */
    d = iget16(RP);
    RPL++;
    if (sr == 0)
      ea = (crsl[BR+br] & 0xFFFF0000) | ((crsl[BR+br] + d) & 0xFFFF);
    else
      ea = (crsl[BR+br] & 0xFFFF0000) | ((crsl[BR+br] + d + crs[sr*2]) & 0xFFFF);
    return ea | ring;

  case 2:  /* TM=2: Indirect and Indirect Preindexed */
    d = iget16(RP);
    RPL++;
    if (sr == 0)
      ea = (crsl[BR+br] & 0xFFFF0000) | ((crsl[BR+br] + d) & 0xFFFF);
    else
      ea = (crsl[BR+br] & 0xFFFF0000) | ((crsl[BR+br] + d + crs[sr*2]) & 0xFFFF);
    ip = get32(ea | ring);
    if (ip & 0x80000000)
      fault(POINTERFAULT, ip>>16, ea);
    return ip | ring;

  case 3:  /* TM=3: Indirect and Indirect Postindexed */
    d = iget16(RP);
    RPL++;
    ea = (crsl[BR+br] & 0xFFFF0000) | ((crsl[BR+br] + d) & 0xFFFF);
    ip = get32(ea | ring);
    if (ip & 0x80000000)
      fault(POINTERFAULT, ip>>16, ea);
    if (sr > 0)
      ip = (ip & 0xFFFF0000) | ((ip + crs[sr*2]) & 0xFFFF);
    return ip | ring;

  default:
    fatal("ea32i: tm out of range!");
  }
  fatal("ea32i: main switch fall through");
#else
  fatal("ea32i: not implemented");
#endif
}
