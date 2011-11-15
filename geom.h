  /* static structure for disk file suffixes and config data */

#ifdef DEMO
#define MAXDRIVES 1
#define MAXCTRL 1     /* 1 controller supported at device address '26 */

#define NUMGEOM 7

  static struct {
    short model;
    char suffix[5];
    short heads;
    short spt;
    short maxtrack;
  } geom[NUMGEOM] = {
     1,  "80M",  5,   9,  823,
     4,  "68M",  3,   9, 1120,
     5, "158M",  7,   9, 1120,
     6, "160M", 10,   9,  822,
    10,  "84M",  5,   8, 1016,   /* MODEL_4714 */
    11,  "60M",  4,   7, 1020,   /* MODEL_4711 */
    12, "120M",  8,   7, 1020,   /* MODEL_4715 */
};

#ifdef __LITTLE_ENDIAN__
  static int geomcksum = 0x15488c73;
#else
  static int geomcksum = 0x156bbb96;
#endif

#else

#define NUMGEOM 25
#define MAXDRIVES 8
#define MAXCTRL 8

  static struct {
    short model;
    char suffix[5];
    short heads;
    short spt;
    short maxtrack;
  } geom[NUMGEOM] = {
     1,  "80M",  5,   9,  823,
     1, "300M", 19,   9,  823,
     2,  "CMD", 21,   9,  823,
     4,  "68M",  3,   9, 1120,
     5, "158M",  7,   9, 1120,
     6, "160M", 10,   9,  822,
     7, "675M", 40,   9,  842,
     7, "600M", 40,   9,  842,
     9, "315M", 19,   9,  823,   /* MODEL_4475 */
    10,  "84M",  5,   8, 1016,   /* MODEL_4714 */
    11,  "60M",  4,   7, 1020,   /* MODEL_4711 */
    12, "120M",  8,   7, 1020,   /* MODEL_4715 */
    13, "496M", 24,  14,  712,   /* MODEL_4735 */
    14, "258M", 17,   6, 1221,   /* MODEL_4719 */
    15, "770M", 23,  19,  850,   /* MODEL_4845 */
    16, "1.1G", 27,  19, 1022,   /* MODEL_4935 */
    17, "328A", 12,   8, 1641,   /* MODEL_4721 */
    17, "328B", 31, 254,   20,   /* MODEL_4721 (7210 SCSI controller) */
    18, "817M", 15,  19, 1381,   /* MODEL_4860 */
    19, "673M", 31, 254,   42,   /* MODEL_4729 */
    20, "213M", 31, 254,   14,   /* MODEL_4730 */
    22, "421M", 31, 254,   26,   /* MODEL_4731 */
    23, "1.3G", 31, 254,   82,   /* MODEL_4732 */
    24, "1G",   31, 254,   65,   /* MODEL_4734 */
    25, "2G",   31, 254,  122,   /* MODEL_4736 */
};
#endif
