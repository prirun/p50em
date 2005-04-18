/* ERRD.INS.CC, SYSCOM, PRIMOS GROUP, 01/15/92
   Mnemonic error codes for Primos file system (CC)
   Copyright (c) 1991, Prime Computer, Inc., Natick, MA 01760 */
/*                     All Rights Reserved */

/* Description:
  
   Abnormal conditions:
  
   Implementation:
    Adding a code requires changes to: ERRD.INS.@@, ERRD.DEF.MOD, KS>ERRCOM.PMA.
    ERRD.INS.CC and ERRD.INS.PASCAL are built from this file, so all
    definitions must by contained on ONE line and all symbols should
    be lower case (CC is case sensitive).
  
    WARNING:  error code definitions MUST start in column 1 not span more
              than one line. Reason: syscom.build.cpl depends on these
              two things.
  
   Modifications:
     Date   Programmer     Description of modification
   01/15/92 A. Griffith    Added error codes E$PNIR, E$FDER, and E$FDIR for
                            On_Line Fix_Disk Gates.
   11/18/91 Sager          Added E$METL for message 512 support
   06/21/91 Gorton         Added E$PWRU and E$CPLE for CHG$PW.
   06/13/91 RLove          Added E$ACDD for ASNDE$.
   06/12/91 RLove          Added E$ACPG for ASSIGN_DISK.
   06/11/91 Peterson       Added e$bfmt.
   02/20/91 A. Griffith    Added E$DNTA for RAS$RA_TO_PATH.
   12/07/90 Snay           Added E$NDSM for disk tracking project.
   10/02/90 Tung           Added error codes E$NOWR, E$ICUT and E$IANL
                           for UT$COPY.
   09/04/90 A. Conte       Added E$UNLI, E$MULI, E$CWAT, E$UBW
   08/30/90 Hunt           Added E$APSM for Acl Protected Memory.
   08/21/90 Nakano         Realigned e$xxx definitions to start in column 1
   06/20/90 Slutz          Added e$ernf.
   05/24/90 Slutz          Added e$nd1s, e$umle.
   04/17/89 Huber          Added error codes for the Name Service: E$RXMH,
                           E$NNET, E$MTPT, E$RPMH, E$MNSH, E$IPTR, E$PTHU,
                           E$IGMT, E$IROO, E$NEWF, E$BPOR, E$PRVT.
   10/21/88 Fisher         Added e$nadm.
   06/15/88 Cook           Converted to SPL.
   12/21/87 Roper          Added e$zero.
   11/03/87 Van Seters     Added e$ista.
   10/06/87 Ng             Added comment on ERRD.DEF.MOD.
   09/15/87 Slutz          Added e$bmpc.
   09/03/87 Y.K.Yang       Added e$dnts and e$snts.
   07/22/87 Tsang          Added e$ok.
   07/08/87 Dossett        Added E$GPON and E$NGPW.
   06/02/87 Hornbaker      Added e$bchk and e$expd.
   05/06/87 A. Conte       Added e$itlb, e$ips, e$dpar, and e$pns.
   04/02/87 Tsang          Added e$rmln.
   02/02/87 Tsang          Added e$lna, e$ldes and e$lny.
   01/20/87 Magnan         Add E$LNP Line Not Present
   11/10/86 Rosenstock     Removed e$nasu.
                           [11/03/86]
   11/10/86 Rosenstock     Removed e$no_nts.
                           [10/23/86]
   11/10/86 A. Conte       Added e$nba, e$lnow.
                           [09/16/86]
   07/21/86 Simon          Added E$NPDA.
   07/21/86 Phillips (UK)  Added E$NINT.
   07/14/86 M. Sadigh      Added a new error code E$REIU for EPF$UNREG.
   07/09/86 Leblang        Rev 21 integrations comprised of:
            James R. Ward  Added e$ variants of Iguana error codes.
            James R. Ward  Added e$ntsh.
            Kiefer         Added declaration for e$nxcb, e$doqf, e$lnoc,e$rqf,
                           e$crej, e$ctmo, e$lhdn, e$ltdn
            James R. Ward  (Pacer Software, Inc.) added NSS and NTS codes:
                           e$nsni, e$nsnc, e$nsac, n$nthn, e$ntns,
                           e$ntst, e$ntcf, e$ntlc, e$ntdl, e$ntin,
                           e$plaa, e$llaa, e$naso, e$nasu, e$ncfg
   07/08/86 Becker/Conte   Add messages for AS$Get/AS$Set/AS$Lin:   e$blin,
                            e$bbuf, e$bpro, e$lnus, e$bfus, e$irbf, e$iabf,
                            e$nnts, e$iasd, e$iasp, e$ilod.
   07/07/86 Milne          Added e$ismr.
   04/06/86 Fichter        Added e$nown, e$blok, e$area.
   03/11/86 JBall          Added e$ndrb and e$cqpt for Q_UPDT use.
   12/30/85 Sadigh/Ng      Added for EPF II: e$ireg, e$inai, e$illn, e$buid,
                             e$inre, e$npsg, e$uinf, e$ivpt, e$snal, e$natf,
                             e$nd3s, e$bsmt, e$ialn, e$bptr, e$idbt, e$bdtr,
                             e$lunr, e$enrg.
   12/27/85 Pinkoski       Added codes for Primix: e$ilus, e$nchd, e$int, e$xshdn,
                           e$nopx, e$nous, e$incom
   12/17/85 Wright         Added e$nres.
   11/27/85 Moore          Added e$imem and e$ifcb for CAM file support.
   11/01/85 Dossett/Yang   Added for Search Rules: e$list, e$rule, e$ntop,
                           e$nest, e$admn, e$eol, e$adrl.
   02/22/85 Cook           Made all definitions occur in lower case on one
                           line so we can generate ERRD.INS.CC and
                           ERRD.INS.PASCAL from this file.  Added e$aele.
   02/13/85 Hornbaker      Added e$aele.
   02/05/85 Bloom          Added e$nrfc.
   01/31/85 Abelli         Added e$cpovfl, e$ioovfl, e$bhovfl.
   01/29/85 Chan           Added E$NOPD.
   01/29/85 Bogardus       Added E$RSHD.
   11/15/84 Pinkoski       Added E$ATNS for dtc/dta support.
   07/06/84 JBall          Added codes for reomte ipc, E$NSLV & E$RSIN.
   04/16/84 Rees           Added E$EXMF, E$BKIO, E$AWER, E$RAMC, E$RIER for
                           contiguous file, block mode, roam support.
   11/30/83 WD Smith       Added e$insufficient_dam_levels & e$past_EOF.
            Slutz          Added e$fidc and e$uafu.
   10/20/83 Kazin          Added E$ALSZ, E$FRER, E$HPER, E$EPFT, E$EPFS, E$ILTD,
                           E$ILTE, E$ECEB, E$EPFL, E$NTA, E$SWPS, E$SWPR, and
                           E$ADCM for EPF support.
   09/25/83 HANTMAN        Added E$BLEF and E$BLET.
   04/29/83 Abelli-Raizen  Added E$UDMA and E$UDMC.
   04/18/83 JBall          Added E$NACC (Not accessable) for IPC support [osi 1566]
   01/21/83 HANTMAN        added the error code E$NSB for decttion of a NSB
                           labelled tape by MAGNET,MAGLIB and LABEL.
   11/20/82 HChen          Added E$IDNF(Slave ID not found).
   11/16/82 Goggin         Added NAMELIST error codes for library error processing.
   10/29/82 HChen          Added E$MNPX (Illegal multiple hops in NPX).
   09/10/82 Kroczak        Added E$RESF (Improper access to a restricted file).
   04/22/82 HChen          Added E$WSLV, E$VCGC and E$MSLV.
   04/04/82 HChen          Added E$APND (for R$BGIN) and E$BVCC.
   03/24/82 Weinberg       Added E$NFAS (not found in attach scan).
   12/14/81 Huber          changed T$GPPI error codes to match rev 18. To do
                           this changed E$RSNU from 137 to 140 and filled in
                           the previously held codes with E$CTPR, E$DFPR, E$DLPR.
   11/06/81 Weinberg       changed codes for ACL rewrite.
   10/26/81 Hornbaker      added F$IO error codes.
   10/22/81 HChen          used the spare one, 137, for E$RSNU.
   05/22/81 Detroy         add T$GPPI error codes.
   04/07/81 Cecchin        merged new errors for Acls  (for Ben Crocker).
   03/25/81 Cecchin        added NPX error codes from 18 to fix mismatch
                           between 18 and 19. Also added spare 18 error codes
                           as a temporary solution.
  ********************************************************************/



/*                                                                           */
/*      CODE DEFINITIONS                                                     */
/*                                                                           */

#define e$ok                    0      /* NORMAL RETURN                          */
#define e$eof                    1      /* END OF FILE                   PE       */
#define e$bof                    2      /* BEGINNING OF FILE             PG       */
#define e$unop                   3      /* UNIT NOT OPEN                 PD,SD    */
#define e$uius                   4      /* UNIT IN USE                   SI       */
#define e$fius                   5      /* FILE IN USE                   SI       */
#define e$bpar                   6      /* BAD PARAMETER                 SA       */
#define e$natt                   7      /* NO UFD ATTACHED               SL,AL    */
#define e$fdfl                   8      /* UFD FULL                      SK       */
#define e$dkfl                   9      /* DISK FULL                     DJ       */
#define e$disk_full              9      /* alias to E$DKFL                        */
#define e$nrit                  10      /* NO RIGHT                      SX       */
#define e$fdel                  11      /* FILE OPEN ON DELETE           SD       */
#define e$ntud                  12      /* NOT A UFD                     AR       */
#define e$ntsd                  13      /* NOT A SEGDIR                  --       */
#define e$dire                  14      /* IS A DIRECTORY                --       */
#define e$fntf                  15      /* (FILE) NOT FOUND              SH,AH    */
#define e$fnts                  16      /* (FILE) NOT FOUND IN SEGDIR    SQ       */
#define e$bnam                  17      /* ILLEGAL NAME                  CA       */
#define e$exst                  18      /* ALREADY EXISTS                CZ       */
#define e$dnte                  19      /* DIRECTORY NOT EMPTY           --       */
#define e$shut                  20      /* BAD SHUTDN (FAM ONLY)         BS       */
#define e$disk                  21      /* DISK I/O ERROR                WB       */
#define e$bdam                  22      /* BAD DAM FILE (FAM ONLY)       SS       */
#define e$ptrm                  23      /* PTR MISMATCH (FAM ONLY)       PC,DC,AC */
#define e$rec_hdr_ptr_mismatch  23      /* alias to E$PTRM                        */
#define e$bpas                  24      /* BAD PASSWORD (FAM ONLY)       AN       */
#define e$bcod                  25      /* BAD CODE IN ERRVEC            --       */
#define e$btrn                  26      /* BAD TRUNCATE OF SEGDIR        --       */
#define e$oldp                  27      /* OLD PARTITION                 --       */
#define e$bkey                  28      /* BAD KEY                       --       */
#define e$bunt                  29      /* BAD UNIT NUMBER               --       */
#define e$bsun                  30      /* BAD SEGDIR UNIT               SA       */
#define e$suno                  31      /* SEGDIR UNIT NOT OPEN          --       */
#define e$nmlg                  32      /* NAME TOO LONG                 --       */
#define e$sder                  33      /* SEGDIR ERROR                  SQ       */
#define e$bufd                  34      /* BAD UFD                       --       */
#define e$bfts                  35      /* BUFFER TOO SMALL              --       */
#define e$fitb                  36      /* FILE TOO BIG                  --       */
#define e$null                  37      /* (NULL MESSAGE)                --       */
#define e$irem                  38      /* ILL REMOTE REF                --       */
#define e$dviu                  39      /* DEVICE IN USE                 --       */
#define e$rldn                  40      /* REMOTE LINE DOWN              --       */
#define e$fuiu                  41      /* ALL REMOTE UNITS IN USE       --       */
#define e$dns                   42      /* DEVICE NOT STARTED            --       */
#define e$tmul                  43      /* TOO MANY UFD LEVELS           --       */
#define e$fbst                  44      /* FAM - BAD STARTUP             --       */
#define e$bsgn                  45      /* BAD SEGMENT NUMBER            --       */
#define e$fifc                  46      /* INVALID FAM FUNCTION CODE     --       */
#define e$tmru                  47      /* MAX REMOTE USERS EXCEEDED     --       */
#define e$nass                  48      /* DEVICE NOT ASSIGNED           --       */
#define e$bfsv                  49      /* BAD FAM SVC                   --       */
#define e$semo                  50      /* SEM OVERFLOW                  --       */
#define e$ntim                  51      /* NO TIMER                      --       */
#define e$fabt                  52      /* FAM ABORT                     --       */
#define e$fonc                  53      /* FAM OP NOT COMPLETE           --       */
#define e$npha                  54      /* NO PHANTOMS AVAILABLE         -        */
#define e$room                  55      /* NO ROOM                       --       */
#define e$wtpr                  56      /* DISK WRITE-PROTECTED          JF       */
#define e$itre                  57      /* ILLEGAL TREENAME              FE       */
#define e$famu                  58      /* FAM IN USE                    --       */
#define e$tmus                  59      /* MAX USERS EXCEEDED            --       */
#define e$ncom                  60      /* NULL_COMLINE                  --       */
#define e$nflt                  61      /* NO_FAULT_FR                   --       */
#define e$stkf                  62      /* BAD STACK FORMAT              --       */
#define e$stks                  63      /* BAD STACK ON SIGNAL           --       */
#define e$noon                  64      /* NO ON UNIT FOR CONDITION      --       */
#define e$crwl                  65      /* BAD CRAWLOUT                  --       */
#define e$crov                  66      /* STACK OVFLO DURING CRAWLOUT   --       */
#define e$crun                  67      /* CRAWLOUT UNWIND FAIL          --       */
#define e$cmnd                  68      /* BAD COMMAND FORMAT            --       */
#define e$rchr                  69      /* RESERVED CHARACTER            --       */
#define e$nexp                  70      /* CANNOT EXIT TO COMMAND PROC   --       */
#define e$barg                  71      /* BAD COMMAND ARG               --       */
#define e$csov                  72      /* CONC STACK OVERFLOW           --       */
#define e$nosg                  73      /* SEGMENT DOES NOT EXIST        --       */
#define e$trcl                  74      /* TRUNCATED COMMAND LINE        --       */
#define e$ndmc                  75      /* NO SMLC DMC CHANNELS          --       */
#define e$dnav                  76      /* DEVICE NOT AVAILABLE         DPTX      */
#define e$datt                  77      /* DEVICE NOT ATTACHED           --       */
#define e$bdat                  78      /* BAD DATA                      --       */
#define e$blen                  79      /* BAD LENGTH                    --       */
#define e$bdev                  80      /* BAD DEVICE NUMBER             --       */
#define e$qlex                  81      /* QUEUE LENGTH EXCEEDED         --       */
#define e$nbuf                  82      /* NO BUFFER SPACE               --       */
#define e$inwt                  83      /* INPUT WAITING                 --       */
#define e$ninp                  84      /* NO INPUT AVAILABLE            --       */
#define e$dfd                   85      /* DEVICE FORCIBLY DETACHED      --       */
#define e$dnc                   86      /* DPTX NOT CONFIGURED           --       */
#define e$sicm                  87      /* ILLEGAL 3270 COMMAND          --       */
#define e$sbcf                  88      /* BAD 'FROM' DEVICE             --       */
#define e$vkbl                  89      /* KBD LOCKED                    --       */
#define e$via                   90      /* INVALID AID BYTE              --       */
#define e$vica                  91      /* INVALID CURSOR ADDRESS        --       */
#define e$vif                   92      /* INVALID FIELD                 --       */
#define e$vfr                   93      /* FIELD REQUIRED                --       */
#define e$vfp                   94      /* FIELD PROHIBITED              --       */
#define e$vpfc                  95      /* PROTECTED FIELD CHECK         --       */
#define e$vnfc                  96      /* NUMERIC FIELD CHECK           --       */
#define e$vpef                  97      /* PAST END OF FIELD             --       */
#define e$virc                  98      /* INVALID READ MOD CHAR         --       */
#define e$ivcm                  99      /* INVALID COMMAND               --       */
#define e$dnct                 100      /* DEVICE NOT CONNECTED          --       */
#define e$bnwd                 101      /* BAD NO. OF WORDS              --       */
#define e$sgiu                 102      /* SEGMENT IN USE                --       */
#define e$nesg                 103      /* NOT ENOUGH SEGMENTS (VINIT$)  --       */
#define e$sdup                 104      /* DUPLICATE SEGMENTS (VINIT$)   --       */
#define e$ivwn                 105      /* INVALID WINDOW NUMBER         --       */
#define e$wain                 106      /* WINDOW ALREADY INITIATED      --       */
#define e$nmvs                 107      /* NO MORE VMFA SEGMENTS         --       */
#define e$nmts                 108      /* NO MORE TEMP SEGMENTS         --       */
#define e$ndam                 109      /* NOT A DAM FILE                --       */
#define e$nova                 110      /* NOT OPEN FOR VMFA             --       */
#define e$necs                 111      /* NOT ENOUGH CONTIGUOUS SEGMENTS         */
#define e$nrcv                 112      /* REQUIRES RECEIVE ENABLED      --       */
#define e$unrv                 113      /* USER NOT RECEIVING NOW        --       */
#define e$ubsy                 114      /* USER BUSY, PLEASE WAIT        --       */
#define e$udef                 115      /* USER UNABLE TO RECEIVE MESSAGES        */
#define e$uadr                 116      /* UNKNOWN ADDRESSEE             --       */
#define e$prtl                 117      /* OPERATION PARTIALLY BLOCKED   --       */
#define e$nsuc                 118      /* OPERATION UNSUCCESSFUL        --       */
#define e$nrob                 119      /* NO ROOM IN OUTPUT BUFFER      --       */
#define e$nete                 120      /* NETWORK ERROR ENCOUNTERED     --       */
#define e$shdn                 121      /* DISK HAS BEEN SHUT DOWN       FS       */
#define e$unod                 122      /* UNKNOWN NODE NAME (PRIMENET)           */
#define e$ndat                 123      /* NO DATA FOUND                 --       */
#define e$enqd                 124      /* ENQUED ONLY                   --       */
#define e$phna                 125      /* PROTOCOL HANDLER NOT AVAIL   DPTX      */
#define e$iwst                 126      /* E$INWT ENABLED CONFIG     DPTX      */
#define e$bkfp                 127      /* BAD KEY FOR THIS PROTOCOL    DPTX      */
#define e$bprh                 128      /* BAD PROTOCOL HANDLER (TAT)   DPTX      */
#define e$abti                 129      /* I/O ABORT IN PROGRESS        DPTX      */
#define e$ilff                 130      /* ILLEGAL DPTX FILE FORMAT     DPTX      */
#define e$tmed                 131      /* TOO MANY EMULATE DEVICES     DPTX      */
#define e$danc                 132      /* DPTX ALREADY CONFIGURED      DPTX      */
#define e$nenb                 133      /* REMOTE MODE NOT ENABLED       NPX      */
#define e$nsla                 134      /* NO NPX SLAVE AVAILABLE        ---      */
#define e$pntf                 135      /* PROCEDURE NOT FOUND          R$CALL    */
#define e$sval                 136      /* SLAVE VALIDATION ERROR       R$CALL    */
#define e$iedi                 137      /* I/O error or device interrupt (GPPI)   */
#define e$wmst                 138      /* Warm start happened (GPPI)             */
#define e$dnsk                 139      /* A pio instruction did not skip (GPPI)  */
#define e$rsnu                 140      /* REMOTE SYSTEM NOT UP         R$CALL    */
#define e$s18e                 141

/*                                                                   */
/* New error codes for REV 19 begin here:                            */
/*                                                                   */

#define e$nfqb                 142      /* NO FREE QUOTA BLOCKS          --       */
#define e$mxqb                 143      /* MAXIMUM QUOTA EXCEEDED        --       */
#define e$max_quota_exceeded 143        /* alias to E$MXQB                        */
#define e$noqd                 144      /* NOT A QUOTA DISK (RUN VFIXRAT)         */
#define e$qexc                 145      /* SETTING QUOTA BELOW EXISTING USAGE     */
#define e$imfd                 146      /* Operation illegal on MFD               */
#define e$nacl                 147      /* Not an ACL directory                   */
#define e$pnac                 148      /* Parent not an ACL directory            */
#define e$ntfd                 149      /* Not a file or directory                */
#define e$iacl                 150      /* Entry is an ACL                        */
#define e$ncat                 151      /* Not an access category                 */
#define e$lrna                 152      /* Like reference not available           */
#define e$cpmf                 153      /* Category protects MFD                  */
#define e$acbg                 154      /* ACL too big                            */
#define e$acnf                 155      /* Access category not found              */
#define e$lrnf                 156      /* Like reference not found               */
#define e$bacl                 157      /* BAD ACL                                */
#define e$bver                 158      /* BAD VERSION                            */
#define e$ninf                 159      /* NO INFORMATION                         */
#define e$catf                 160      /* Access category found (Ac$rvt)         */
#define e$adrf                 161      /* ACL directory found (Ac$rvt)           */
#define e$nval                 162      /* Validation error (nlogin)              */
#define e$logo                 163      /* Logout (code for fatal$)               */
#define e$nutp                 164      /* No unit table available. (PHANT$)      */
#define e$utar                 165      /* Unit table already returned. (UTDALC)  */
#define e$uniu                 166      /* Unit table not in use. (RTUTBL)        */
#define e$nfut                 167      /* No free unit table. (GTUTBL)           */
#define e$uahu                 168      /* User already has unit table. (UTALOC)  */
#define e$panf                 169      /* Priority ACL not found.                */
#define e$misa                 170      /* Missing argument to command.           */
#define e$sccm                 171      /* System console command only.           */
#define e$brpa                 172      /* Bad remote password          R$CALL    */
#define e$dtns                 173      /* Date and time not set yet.             */
#define e$spnd                 174      /* REMOTE PROCEDURE CALL STILL PENDING    */
#define e$bcfg                 175      /* NETWORK CONFIGURATION MISMATCH         */
#define e$bmod                 176      /* Illegal access mode  (AC$SET)          */
#define e$bid                  177      /* Illegal identifier   (AC$SET)          */
#define e$st19                 178      /* Operation illegal on pre-19 disk       */
#define e$ctpr                 179      /* Object is category-protected (Ac$chg)  */
#define e$dfpr                 180      /* Object is default-protected (Ac$chg)   */
#define e$dlpr                 181      /* File is delete-protected (Fil$dl)      */
#define e$blue                 182      /* Bad LUBTL entry              (F$IO)    */
#define e$ndfd                 183      /* No driver for device         (F$IO)    */
#define e$wft                  184      /* Wrong file type              (F$IO)    */
#define e$fdmm                 185      /* Format/data mismatch         (F$IO)    */
#define e$fer                  186      /* Bad format                   (F$IO)    */
#define e$bdv                  187      /* Bad dope vector              (F$IO)    */
#define e$bfov                 188      /* F$IOBF overflow              (F$IO)    */
#define e$nfas                 189      /* Top-level dir not found or inaccessible*/
#define e$apnd                 190      /* Asynchronous procedure still pending   */
#define e$bvcc                 191      /* Bad virtual circuit clearing           */
#define e$resf                 192      /* Improper access to a restricted file   */
#define e$mnpx                 193      /* Illegal multiple hops in NPX.          */
#define e$synt                 194      /* SYNTanx error                          */
#define e$ustr                 195      /* Unterminated STRing                    */
#define e$wns                  196      /* Wrong Number of Subscripts             */
#define e$ireq                 197      /* Integer REQuired                       */
#define e$vng                  198      /* Variable Not in namelist Group         */
#define e$sor                  199      /* Subscript Out of Range                 */
#define e$tmvv                 200      /* Too Many Values for Variable           */
#define e$esv                  201      /* Expected String Value                  */
#define e$vabs                 202      /* Variable Array Bounds or Size          */
#define e$bclc                 203      /* Bad Compiler Library Call              */
#define e$nsb                  204      /* NSB tape was detected                  */
#define e$wslv                 205      /* Slave's ID mismatch                    */
#define e$vcgc                 206      /* The virtual circuit got cleared.       */
#define e$mslv                 207      /* Exceeds max number of slaves per user  */
#define e$idnf                 208      /*  Slave's ID not found                  */
#define e$nacc                 209      /* Not accessible                         */
#define e$udma                 210      /* Not Enough DMA channels                */
#define e$udmc                 211      /* Not Enough DMC channels                */
#define e$blef                 212      /* Bad tape record length and EOF         */
#define e$blet                 213      /* Bad tape record length and EOT         */
#define e$alsz                 214      /* Allocate request too small             */
#define e$frer                 215      /* Free request with invalid pointer      */
#define e$hper                 216      /* User storage heap is corrupted         */
#define e$epft                 217      /* Invalid EPF type                       */
#define e$epfs                 218      /* Invalid EPF search type                */
#define e$iltd                 219      /* Invalid EPF LTD linkage descriptor     */
#define e$ilte                 220      /* Invlaid EPF LTE linkage discriptor     */
#define e$eceb                 221      /* Exceeding command environment breadth  */
#define e$epfl                 222      /* EPF file exceeds file size limit       */
#define e$nta                  223      /* EPF file not active for this user      */
#define e$swps                 224      /* EPF file suspended within program session */
#define e$swpr                 225      /* EPF file suspended within this process */
#define e$adcm                 226      /* System administrator command ONLY      */
#define e$uafu                 227      /* Unable to allocate file unit           */
#define e$unable_to_allocate_file_unit   227      /* alias to E$UAFU              */
#define e$fidc                 228      /* File inconsistent data count           */
#define e$file_inconsistent_data_count   228      /* alias to e$fidc              */
#define e$indl                 229      /* alias to e$insufficient_dam_level      */
#define e$insufficient_dam_levels   229 /* Not enough dam index levels as needed  */
#define e$peof                 230      /* alias to e$past_EOF                    */
#define e$past_eof             230      /* Past End Of File                       */
#define e$exmf                 231      /* Extent map full                        */
#define e$bkio                 232      /* Unit open for block mode i/o           */
#define e$awer                 233      /* Asynchronous write error               */
#define e$ramc                 234      /* R0AM access mode conflict              */
#define e$rier                 235      /* R0AM internal error                    */
#define e$nslv                 236      /* Process not a slave                    */
#define e$rsin                 237      /* Remote system has initialized.         */
#define e$atns                 238      /* Attribute not supported                */
#define e$rshd                 239      /* Remote disk has been shut down.        */
#define e$nopd                 240      /* No paging device defined.              */
#define e$nrfc                 241      /* Specified reverse flow control on AMLC */
#define e$cpovfl               242      /* PX$SVTIM overflow of CPU seconds       */
#define e$ioovfl               243      /* PX$SVTIM overflow of I/O seconds       */
#define e$bhovfl               244      /* PX$SVTIM overflow both CPU & I/O sec.s */
#define e$aele                 245      /* Attempt to execute non-executable library */
#define e$list                 246      /* Search list not found or invalid.      */
#define e$rule                 247      /* Search rule not found or invalid.      */
#define e$ntop                 248      /* Search rule was not an optional rule.  */
#define e$nest                 249      /* Template files were nested too deeply--probably circular reference. */
#define e$admn                 250      /* Illegal attempt to change administrator rule. */
#define e$eol                  251      /* End of list reached.                   */
#define e$adrl                 252      /* Error in administrator rules.          */
#define e$ifcb                 253      /* Insufficient free contiguous blocks.   */
#define e$imem                 254      /* Insufficient memory for extent map.    */
#define e$nres                 255      /* No resources available for request.    */
#define e$ilus                 256      /* Illegal use of Primix gate             */
#define e$nchd                 257      /* No child found for this process.       */
#define e$int                  258      /* Wait terminated by interrupt           */
#define e$xshdn                259      /* Can NOT initialize Primix when running */
#define e$nopx                 260      /* Can NOT shutdown Primix when NOT running */
#define e$nous                 261      /* Primix process table has no users when it should have users. */
#define e$incom                262      /* Primix process table returned is incomplete */
#define e$ireg                 263      /* Illegal EPF Registration */
#define e$inai                 264      /* Invalid number of args. in initialization routine */
#define e$illn                 265      /* Illegal link at registration */
#define e$buid                 266      /* Bad user id */
#define e$inre                 267      /* Invalid request */
#define e$npsg                 268      /* Not enough per-user DATR1 segments */
#define e$uinf                 269      /* User Id not found */
#define e$ivpt                 270      /* An invalid block pointer was given */
#define e$snal                 271      /* Segment not allocated */
#define e$natf                 272      /* Not able to free storage */
#define e$nd3s                 273      /* No Dtar 3 Segments available */
#define e$bsmt                 274      /* Null smt_ptr or bad field within SMT */
#define e$ialn                 275      /* Illegal alias name */
#define e$bptr                 276      /* Bad pointer (within SMT?) */
#define e$idbt                 277      /* Illegal database */
#define e$bdtr                 278      /* Bad DTAR */
#define e$lunr                 279      /* Library unregistered */
#define e$enrg                 280      /* EPF has not been registered */
#define e$ndrb                 281      /* No directory block for unit.           */
#define e$cqpt                 282      /* Circular Quota parent thread.          */
#define e$area                 283      /* Corrupted area encountered.            */
#define e$nown                 284      /* Not owner of resource.                 */
#define e$blok                 285      /* Bad block encountered.                 */
#define e$ismr                 286      /* Invalid static mode resume.            */
#define e$blin                 287      /* Bad line number.                       */
#define e$bbuf                 288      /* Bad buffer number                      */
#define e$bprotocol            289      /* Bad protocol                           */
#define e$line_in_use          290      /* Line in use                            */
#define e$buf_in_use           291      /* Buffer in use                          */
#define e$invalid_rem_buf_use   292     /* Invalid use of remote buffer           */
#define e$invalid_asline_buf_use   293  /* Invalid use of assign line buffer      */
#define e$invalid_asd_use      294      /* Invalid ASD use                        */
#define e$invalid_asd_sample_speed   295  /* Invalid sample speed for ASD         */
#define e$invalid_dislog_use   296      /* Invalid use of DISLOG                  */
#define e$nsni                 297      /* nss database not initialized           */
#define e$nsnc                 298      /* nss database naming conflict           */
#define e$nsac                 299      /* nss database address conflict          */
#define e$nthn                 300      /* nts host not configured                */
#define e$ntns                 301      /* nts not started                        */
#define e$ntst                 302      /* nts already started                    */
#define e$ntcf                 303      /* not an nts configuration file          */
#define e$ntlc                 304      /* LHC is unconfigured                    */
#define e$ntin                 305      /* nts database is uninitialized          */
#define e$ntdl                 306      /* LHC is not down-line loaded            */
#define e$plaa                 307      /* primos line already assoc              */
#define e$llaa                 308      /* lts line already assoc                 */
#define e$naso                 309      /* not associated                         */
#define e$ncfg                 310      /* not configured                         */
#define e$nxcb                 311      /* XCB unavailable for request            */
#define e$doqf                 312      /* Device output queue full               */
#define e$lnoc                 313      /* Line not connected                     */
#define e$rqf                  314      /* Request queue full                     */
#define e$crej                 315      /* Connection rejected                    */
#define e$ctmo                 316      /* Connection request timed out           */
#define e$lhdn                 317      /* LHC down                               */
#define e$ltdn                 318      /* LTS down                               */
#define e$ntsh                 319      /* NTS shutdown                           */
#define e$qful                 320      /* Queue is full                          */
#define e$qemp                 321      /* Queue is empty                         */
#define e$noq                  322      /* No queue for queue operation           */
#define e$val                  323      /* Validation error                       */
#define e$comm                 324      /* Command illegal for this operation     */
#define e$awir                 325      /* Page is already wired                  */
#define e$iwir                 326      /* Page is not wired                      */
#define e$npda                 327      /* No password directories allowed        */
#define e$nint                 328      /* System not initialized                 */
#define e$reiu                 329      /* Registered EPF is in-use.              */
#define e$nba                  330      /* No Buffers Available                   */
#define e$lnow                 331      /* Line Not Owned By You                  */
#define e$lnp                  332      /* Line Not Present on System             */
#define e$lna                  333      /* Lock not allocated                     */
#define e$ldes                 334      /* Lock has been destroyed                */
#define e$lny                  335      /* Lock is not yours                      */
#define e$rmln                 336      /* Illegal operation on remote line.      */
#define e$itlb                 337      /* Invalid use of terminal line buffer.   */
#define e$ips                  338      /* Invalid parameter setting.             */
#define e$dpar                 339      /* Duplicate parameter.                   */
#define e$pns                  340      /* Parameter not settable.                */
#define e$bchk                 341      /* Bad checksum                           */
#define e$expd                 342      /* Software has expired                   */
#define e$dnts                 343      /* Density not selected.                  */
#define e$snts                 344      /* Speed not selected.                    */
#define e$bmpc                 345      /* Magtape controller hung.               */
#define e$gpon                 346      /* Generated Passwords ON.                */
#define e$ngpw                 347      /* No Generated PassWords (they're not on).*/
#define e$ista                 348      /* Invalid state */
#define e$zero                 349      /* Uninitialized block on robust part     */
#define e$nadm                 350      /* System Not Admitting Users */
#define e$rxmh                 351      /* Multiple NPX hop on ext dir entry(root)*/
#define e$nnet                 352      /* The Network isn't running              */
#define e$mtpt                 353      /* Operation illegal on Mount-Point       */
#define e$rpmh                 354      /* Multiple NPX hop on portals            */
#define e$mnsh                 355      /* Multiple name space hop                */
#define e$iptr                 356      /* Null pointer given.                    */
#define e$pthu                 357      /* Pathname unavailable.                  */
#define e$igmt                 358      /* Inconsistent GMTs on different machines*/
#define e$iroo                 359      /* Operation illegal on root directory.   */
#define e$newf                 360      /* Primos rev on remote machine too old.  */
#define e$bpor                 361      /* Target node of portal is not remote.   */
#define e$prvt                 362      /* Remote reference to a private disk.    */
#define e$nd1s                 363      /* No Dtar 1 Segments available */
#define e$umle                 364      /* Unexpected GetMutexLock error */
#define e$ernf                 365      /* EPF registration level not found */
#define e$apsm                 366      /* Acat currently protecting shared memory*/
#define e$unli                 367      /* Specified user not logged in. */
#define e$muli                 368      /* Multiple users logged in      */
#define e$cwat                 369      /* Can not watch.                */
#define e$ubw                  370      /* User being watched another user. */
#define e$icut                 371      /* Incompatible unit table entry type.    */
#define e$ianl                 372      /* Initial attach point is not local.     */
#define e$nowr                 373      /* Not open for write.                    */
#define e$ndsm                 374      /* No dsm msg required, see proposal  62  */
#define e$dnta                 375      /* Invalid physical disk number (off_line)*/
#define e$bfmt                 376      /* Format not supported on this drive     */
#define e$acpg                 377      /* Cannot assign active paging disk       */
#define e$acdd                 378      /* Cannot assign active crash dump disk   */
#define e$pwru                 379      /* Password recently used.                */
#define e$cple                 380      /* Change Password Limit Exceeded.        */
#define e$metl                 381      /* Messages on remote system limited ..   */
#define e$pnir                 382      /* OnLine Disk Part. NOT being Fix_Disked */
#define e$fder                 383      /* Fatal error del file info, OnLine Fix_D*/
#define e$fdir                 384      /* OnLine Fix_Disk Internal error         */
#define e$last                 384      /* THIS ***MUST*** BE LAST        --      */

/*                                                          */
/*  The value of E$LAST must equal the last error code.     */
/*                                                          */

/* End of ERRD.INS.CC */
