/* KEYS.INS.PL1, SYSCOM, PRIMOS GROUP, 11/18/91
   Mnemonic keys for Primos file system (CC)
   Copyright (c) 1982, Prime Computer, Inc., Natick, MA 01760 */

/* */
/****                                                       ****/
/****  IMPORTANT:  Values in this file MUST be in decimal.  ****/
/****              Bit strings, octal, or hex are no good!  ****/
/****                                                       ****/
/****              All comments should have a               ****/
/****              for each line of the comment.            ****/
/****              Multi-line comments break the conversion ****/
/****              utilities in syscomsrc.                  ****/
/****              The length of the keyname+length of value****/
/****              should not be more than 19 characters.   ****/
/* MODIFICATIONS:
     Date   Programmer     Description of modification
   11/18/91 Sager          Added K$ENCR, K$DSCR, K$MLEN, K$CRLF, K$ENSH,
                           and K$ENLG for Message 512 support.
   04/11/91 Gorton         Added K$CHPR, K$ENCP, K$ENPW, K$DSCP, K$DSPW for
                           for CHANGE_PROJECT.
   12/07/90 Snay           Added k$cra_mismatch,
                           k$set_dta_dtm, and k$rtnrec_error for disk
                           error tracking.
   12/04/90 RRM            added K$elng for long entry(point)names k$elng
   09/04/90 A. Conte       Added k$getf, k$getn, k$gmyw, k$setw, k$strt, and
                           k$stop.
   08/30/90 Hunt           Added K$INT0 and K$DATA for APM.
   08/10/90  J.Pascucci    Fixed comments that broke the .IBAS conversion.
                           and added warning.
   04/10/90 Morin          Added K$NTLK for PRWF$$ key  option mode.
   03/12/90 Slutz          Chagned k$nmnt from 32 to 128.
   10/19/89 Tung           Added K$PEOF for prwf$$ and K$FACR for sgdr$$.
   05/03/89 RRM            Added new key K$NULF for OPSRS$/SRSFX$ null first
                           search.       [4039718] -Rich Malloy
   04/17/89 Huber          Added K$NMNT key for srch$$.
   12/26/88 Slutz          Readded K$RW & K$RWX for VINIT$
                           K$VMRW for SRCH$$ & VINIT$.
   11/02/88 Snay           Added registered_epf support:
                           Added k$force and k$relax.
                           Added k$some and k$spublic0.
                           k$public3_one and k$public0_one.
                           Added k$public_all, k$public_one, k$public3_one
                           and k$public0_one.
                           Added keys k$comm and k$prcd.
                           Added key k$initreg.
                           Added K$PUBLIC keys for EPFs.
   09/23/88 A. Conte       Added K$GTAL for As$Lst gate.
   08/16/88 Vergin         Added new keys for BKP$OP and BKP$SATR.
   06/15/88 Cook           Converted to SPL.
   05/16/88 Phyfe          Added K$URI key for CHBK$$ (for DMD DCM
                           project - the Bayer Cache).
   02/23/88 Allen          Added k$qual to indicate qualified pathnames only
                           for tnchk$().
   12/18/87 Poh            Added k$brief and k$long to indicate brief or long
                           prompt.  It is used in CL$MSG.
   07/17/87 Roper          Remove k$rrsv from search rule keys (unused)
   03/18/87 M. Sadigh      Changed  k$reinit_all_reg to K$reinit_ref_libs.
   01/12/87 A. Conte       Added K$SLS and K$PLST for As$Set gate.
   07/14/86 M. Sadigh      Added a new key K$FRC_UNREG EPF$UNREG.
   07/05/86 Jones          Rev 21.0 integration of:
            Tate           [developed: 05/01/86]
                           Added comment, (new) SNCHK$'s uses xxCHK$ keys.
   06/16/86 Kazin          Deleted dynamic storage manager keys. [SPAR 3019441]
   05/22/86 M. Sadigh      Added k$reinit_all_reg which used when calling
                           EPF$INIT to re-intialize all referneced libraries.
   02/05/86 Silveira       Added K$WIRE for wired class dynamic storage.
   02/04/86 Dossett        Changed keys K$UNKN, K$RRSV, K$ACAT, K$FILE,
                                        K$SDIR, K$DIR
                           from hexadecimal values to decimal to make the
                           build program for SYSCOM>KEYS.INS.@ happy.
   02/03/86 Moore          Revert the values of K$INB and K$OUTB. Fix in TTY$RS.
   12/19/85 M. Sadigh      Added k$reinit_reg for EPF$INIT. Also added K$unreg
   ..................      to unregister a registered epf.
   11/27/85 Moore          Changed K$NCNT to K$NCAM.
   11/25/85 Moore          [3015705] Added RRS fix to keys k$inb, k$outb.
   11/01/85 Dossett/Yang   Added K$(TEXT HMDR ORDR RFDR KEYW ANYTYPE UNKN
                           RRSV ACAT FILE SDIR DIR) for Search Rules primitives
                           (SR$NEXTR, SR$EXSTR, OPSR$ and OPSRS$).
   01/03/85 JBall          Added K$CMWR, K$CMRD for IPC$CM.
   11/16/84 Pinkoski       Added K$BKUP for SRCH$$, K$DTA & K$DTC for SATR$$.
   10/15/84 JSheehan       Added K$LINE, K$LINC, K$LINS, and K$LINU for
                           DS$ASY; and added K$NEXT for DS$UNI.
   08/06/84 RMorris        Added k$inb and k$outb for TTY$RS.
   07/06/84 Sadigh         Added K$COMO to gpath$
   04/16/84 Rees           Added K$NCNT, K$BKIO to SRCH$$ keys.
                           Added K$WAIT,K$SAVE CHBK$$ keys.
   01/16/84 JBall          Added K$MINE for IPC$GU, also added low water key
                           (K$IPCB) for IPC$ST.
   01/12/84 JBall          K$MMSG no longer defined for IPC.
   08/10/83 Kazin          Added k$segn key for GPATH$, added K$NO_FRC_DEL and
                           K$FRC_DEL for DELEPF$, added mods to EPF keys,
                           added K$SPRC for subsystem process class dynamic
                           storage and removed K$VMRW.
   07/14/83 Kroczak        Changed k$dtls (date/time last saved) to
                           dtb (date/time backed-up).
   04/18/83 JBall          Added keys for IPC support. [osi 1566]
   01/27/83 Kazin          Added K$SPEC to VINIT$ keys. [OSINFO 1362]
   01/24/83 Kazin          Added keys for dynamic storage manager. [OSI 1329]
   11/15/82 Kroczak        Added k$trun key to satr$$
   10/11/82 Kroczak        Added k$dtls key to satr$$.
   09/10/82 Kroczak        Added k$ltyp key to satr$$ and k$resv key to srch$$
   07/28/82 Kazin          Deleted K$RW, K$RWX, and K$GATE from VINIT$ keys.
   07/03/82 Weinberg       Added keys for DIR$CR.
   06/17/82 Kazin          Added K$DUPL for vinit$.
   05/21/82 Goggin         Added k$st$s, k$st$n, k$nlop, and k$lonp for
   ........                event logging module LGINI$.PLP.
   04/29/82 Kazin          Added keys for SW$INT.
   12/02/81 Kazin          Got rid of VINIT$ keys k$pref, k$spec, and k$dtar.
   11/18/81 Curreri        Added keys for log_init to turn logging
   ........                or net logging on or off.
   11/12/81 Weinberg       Added k$grp key for Idchk$.
   11/09/81 Weinberg       Removed non-standard ERROR code definitions from
   ........                MGSET$ section; added keys for R/W locks in SATR$$.
   10/06/81 Weinberg       Removed initial attach point keys for ATCH$$,
   ........                ACL keys for RDEN$$; added keys for DIR$RD.
   07/24/81 Kazin          Added allocate consecutive segment keys for GETSN$.
   07/16/81 Weinberg       Added keys for LDISK$.
   06/17/81 Weinberg       Added keys for FNCHK$ and friends.
   04/24/81 Weinberg       Added K$INIA to GPATH$ keys.
   03/11/81 Cecchin        added EPF keys.
   */

/* key definitions                             */

/* *********************** prwf$$ ***********************        */

/* ******** rwkey  ********                       */

#define  k$read               1          /* read                                  */
#define  k$writ               2          /* write                                 */
#define  k$posn               3          /* position only                         */
#define  k$trnc               4          /* truncate                              */
#define  k$rpos               5          /* read current position                 */

/* ******** poskey ********                       */

#define  k$prer               0          /* pre-position relative                 */
#define  k$prea               8          /* pre-position absolute                 */
#define  k$posr               16         /* post-position relative                */
#define  k$posa               24         /* post-position absolute                */
#define  k$peof               128        /* pre-position to eof                   */

/* ******** mode   ********                       */

#define  k$conv               256        /* convenient number of words            */
#define  k$frcw               16384      /* forced write to disk                  */
#define  k$ntlk               8192       /* Avoid taking tran lock for writing    */

/* *********************** srch$$ ***********************        */

/* ******** action ********                        */

/* k$read 1#define                           open for read (k$read) */

/* k$writ 2#define                           open for write (k$writ) */

#define  k$rdwr               3          /* open for read & write                 */
#define  k$clos               4          /* close file                            */
#define  k$dele               5          /* delete file                           */
#define  k$exst               6          /* check file's existence                */
#define  k$bkup               7          /* open for read BY BACKUP utility       */
#define  k$vmr                16         /* open for vmfa read                    */
#define  k$vmrw               48         /* open for vmfa read/write              */
#define  k$bkio               8192       /* open for block mode                   */
#define  k$getu               16384      /* system returns unit number            */
#define  k$resv               -32768     /* reserved bit   */

/* ******** ref    ********                       */

#define  k$iufd               0          /* file entry is in ufd                  */
#define  k$iseg               64         /* file entry is in segment directory    */
#define  k$nmnt               128        /* don't cross mount points */
#define  k$cacc               512        /* change access                         */

/* ******** newfil ********                       */

#define  k$nsam               0          /* new sam file                          */
#define  k$ndam               1024       /* new dam file                          */
#define  k$nsgs               2048       /* new sam segment directory             */
#define  k$nsgd               3072       /* new dam segment directory             */
#define  k$ncam               4096       /* new contiguous file    */
#define  k$curr               -1         /* currently attached ufd                */

/* *********************** vinit$ ***********************        */

#define  k$any                0          /* any segment(s) acceptable             */
#define  k$spec               1          /* use specified segments */
#define  k$dupl               16         /* duplicate segs requested */
#define  k$cnsc               8          /* consecutive segments required         */
#define  k$r                  2          /* Read access on segment (^= k$read!)   */
#define  k$rx                 6          /* Read/execute access                   */
#define  k$rw                 3          /* Read/write access on segment          */
#define  k$rwx                7          /* Read/write/execute access             */

/* *********************** getsn$, find_seg *************        */

#define  k$down               0          /* Allocate decreasing segment #'s */
#define  k$up                 1          /* Allocate increasing segment #'s */
#define  k$upc                2          /* Allocate increas. consec. segs. */
#define  k$dwnc               4          /* Allocate decreas. consec. segs. */

/* *********************** atch$$ ***********************        */

/* ******** key    ********                       */

#define  k$imfd               0          /* ufd is in mfd                         */
#define  k$icur               2          /* ufd is in current ufd                 */

/* ******** keymod ********                       */

#define  k$setc               0          /* set current ufd (do not set home)     */
#define  k$seth               1          /* set home ufd (as well as current)     */

/* ******** name   ********                       */

#define  k$home               0          /* return to home ufd (key=k$imfd)       */

/* ******** ldisk  ********                       */

#define  k$alld               -32768     /* search all disks                      */

/* k$curr -1#define                        search mfd of current disk (k$curr)   */

/* *********************** ac$set ***********************        */

/* k$any 0#define                         Do it regardless */

#define  k$crea               1          /* Create new ACL (error if already exists)  */
#define  k$rep                2          /* Replace existing ACL (error if does not exist)*/

/* *********************** sgdr$$ ***********************        */

/* ******** key    ********                       */

#define  k$spos               1          /* position to entry number in segdir    */
#define  k$gond               2          /* position to end of segdir             */
#define  k$gpos               3          /* return current entry number           */
#define  k$msiz               4          /* make segdir given nr of entries       */
#define  k$mvnt               5          /* move file entry to different position */
#define  k$full               6          /* position to next non-empty position   */
#define  k$free               7          /* position to next free entry           */
#define  k$facr               11         /* find next free entry and create file */

/* *********************** rden$$ ***********************        */

/* ******** key    ********                       */

/* k$read 1#define                        read next entry (k$read)              */

#define  k$rsub               2          /* read next sub-entry                   */

/* k$gpos 3#define                        return current pos in ufd (k$gpos)    */

#define  k$upos               4          /* position in ufd                       */
#define  k$name               5          /* read entry specified BY name          */

/* ******************************* dir$rd                        */

/* k$read 1#define                       Read next entry                        */

#define  k$init               2          /* Initialize directory (read header      */

/* *********************** satr$$ ***********************        */

/* ******** key    ********                       */

#define  k$prot               1          /* set protection                        */
#define  k$dtim               2          /* set datetime modified                 */
#define  k$dmpb               3          /* set dumped bit                        */
#define  k$rwlk               4          /* set per file read/write lock          */
#define  k$sown               5          /* set OWNER field                       */
#define  k$sdl                6          /* set ACL/DELETE switch   */
#define  k$ltyp               7          /* set logical type */
#define  k$dtb                8          /* set date/time backed-up */
#define  k$trun               9          /* set truncated BY FIX_DISK bit */
#define  k$dta                10         /* set date/time last accessed           */
#define  k$dtc                11         /* set date/time created                 */

/* ****** rwlock ******                           */

#define  k$dflt               0          /* Use system default value              */
#define  k$excl               1          /* N readers OR one writer               */
#define  k$updt               2          /* N readers AND one writer              */
#define  k$none               3          /* N readers AND N writers               */

/* *********************** errpr$ ***********************        */

/* ******** key    ********                       */

#define  k$nrtn               0          /* never return to user                  */
#define  k$srtn               1          /* return after start command            */
#define  k$irtn               2          /* immediate return to user              */

/* *********************** limit$ ************************       */

/* ******** key    ********                       */

/* k$read 1#define                        returns information */

/* k$writ 2#define                        sets information */

/* ******** subkey ********                       */

#define  k$cplm               256        /* CPU time in seconds                   */
#define  k$lglm               512        /* login time in minutes                 */

/* *********************** gpath$ ************************       */

/* ******** key    ********                       */

#define  k$unit               1          /* pathname of unit returned             */
#define  k$cura               2          /* pathname of current attach point      */
#define  k$homa               3          /* pathname of home attach point         */
#define  k$inia               4          /* pathname of initial attach point      */
#define  k$segn               5          /* pathname of segment returned */
#define  k$como               6          /* pathname of comoutput file

                                                ************************** ds$uni *************************** */
/* ds$uni uses keys declared for gpath$ plus:                    */

#define  k$next               -1         /* pathname of next open unit */

/* *********************** mgset$/msg$st *****************       */

/* ******** key    ********                       */

#define  k$acpt               0          /* m -accept */
#define  k$defr               1          /* m -defer  */
#define  k$rjct               2          /* m -reject */
#define  k$encr               3          /* Enable CRLF */
#define  k$dscr               4          /* Disable CRLF messages */
#define  k$ensh               5          /* Enable 80 character messages*/
#define  k$enlg               6          /* Enable MAX character messages*/
#define  k$crlf               7          /* query crlf state */
#define  k$mlen               8          /* Query maax message length*/

/* ******************************* fnsid$ ***************************** */

#define  k$list               1          /* List enabled nodes  */
#define  k$add                2          /* Add to existing list */
#define  k$srch               3          /* Search for specific node */

/* ********************************************************************** */

/* ******************* keys for resume functionality for epfs *********** */

/* ************************ str$al, str$fr ****************************** */

#define  k$proc               1          /* storage types: per process storage */
#define  k$level              2          /* per level */
#define  k$prog               3          /* per program */
#define  k$syst               4          /* per system */
#define  k$frblk              5          /* free a blk of storage */
#define  k$anywhere           -1         /* base the storage block anywhere */
#define  k$zero               0          /* base the block at word zero */

/* ********************* epf$map, epf$init, epf$allc *************************
   ********************* epf$run, epf$invk, epf$del  *************************
   ********************* delepf$ ******************************************* */

#define  k$copy               1          /* copy epf file into temp segs */
#define  k$dbg                2          /* map dbg info into memory from epf */
#define  k$initall            1          /* init all of the linkage areas */
#define  k$reinit             2          /* only reinit linkage areas */
#define  k$invk               0          /* invoke and do not delete epf from memory */
#define  k$invk_del           2          /* invoke and delete EPF */
#define  k$restore_only       1          /* restore but do not invoke EPF */
#define  k$reinit_all_reg     3          /* re-initialize per-user linkage
                                                of registered EPF and all
                                                its referenced libraries */
#define  k$reinit_reg         4          /* re-initalize only the registerd
                                                library */
#define  k$initreg            5          /* initialize registered EPF */
#define  k$frc_del            1          /* force terminate EPF */
#define  k$duct               4          /* decrement user count on registered epf */
#define  k$unreg              5          /* unregister the EPF. */
#define  k$frc_unreg          6          /* force unregister the EPF. */
#define  k$no_frc_del         0          /* do not force terminate EPF */
#define  k$nonregister        0          /* do not register this EPF*/
#define  k$public             1          /* register as ring 3 non-system class EPF */
#define  k$spublic0           2          /* register as ring 0 system class EPF */
#define  k$comm               1          /* dynamic linking to common */
#define  k$prcd               2          /* dynamic linking to proc  */
#define  k$some               1          /* report only matches */
#define  k$force              1          /* force register an epf */
#define  k$relax              2          /* do not force register an epf */

/* **************** fnchk$, tnchk$, idchk$, pwchk$, snchk$ **************** */

#define  k$uprc               1          /* Mask to uppercase */
#define  k$wldc               2          /* Allow wildcards (not pwchk$, snchk$) */
#define  k$null               4          /* Allow null names */
#define  k$num                8          /* Allow numeric names (fnchk$ only) */
#define  k$grp                8          /* Check group name (idchk$) */
#define  k$qual               16         /* Allow qualified names only (tnchk$ only) */
#define  k$elng               32         /* Allow long entry(point)names (fnchk$ only) */

/* ******************************** q$set ********************************* */

#define  k$smax               1          /* Set max quota */

/* ******************************* LGINI$ *****************************/

#define  k$lof                0          /* logging off */
#define  k$nlof               1          /* net logging off */
#define  k$lon                2          /* logging on, use today's date */
#define  k$nlon               3          /* net logging on, use today's dt */
#define  k$lonp               4          /* turn sys logging on, use */

/* saved logging file date */

#define  k$nlop               5          /* turn net logging on, use */

/* saved logging file date */

#define  k$st$s               6          /* return status of sys logging */
#define  k$st$n               7          /* return status of net logging */

/* ******************************* ldisk$ ********************************* */

#define  k$all                0          /* Return all disks */
#define  k$locl               1          /* Local disks only */
#define  k$rem                2          /* Remote disks only */
#define  k$sys                3          /* Disks from specified system only */

/* ******************************* sw$int ********************************* */

/* k$read 1#define                           Read present status */

#define  k$on                 2          /* Turn on interrupt(s) */
#define  k$off                3          /* Turn off interrupt(s) */
#define  k$rdon               4          /* Read present status and
                                                turn on interrupt(s) */
#define  k$rdof               5          /* Read present status and
                                                turn off interrupt(s) */
#define  k$rdal               6          /* Read present status of all
                                                interrupts */
#define  k$alon               7          /* Turn on all interrupts */
#define  k$alof               8          /* Turn off all interrupts */
#define  k$raon               9          /* Read present status and
                                                turn on all interrupts */
#define  k$raof               10         /* Read present status and
                                                turn off all interrupts */

/* ******************************* dir$cr ********************************* */

#define  k$same               0          /* Create directory of parent's type */
#define  k$pwd                1          /* Create password directory */

/* ********************************** ipc ********************************** */

/* ******************************** ipc_ckac ******************************** */

/* k$any 0#define        Check mailbox user ID for any access      */

/* k$read 1#define       Check mailbox user ID for read access     */

/* k$writ 2#define       Check mailbox user ID for write access    */

#define  k$semt               3          /* Check for semaphore type open             */
#define  k$intt               4          /* Check for interrupt type open             */

/* ******************************** ipc$cm ********************************** */

#define  k$cmwr               1          /* change mode to write only */
#define  k$cmrd               2          /* change mode to read only */

/* ******************************** ipc$gu ********************************** */

/* k$read 1#define       Get user IDs for reading                  */

/* k$writ 2#define       Get user IDs for writing                  */

/* k$rdwr 3#define       Get user IDs for reading and writing      */

#define  k$mine               4          /* Get my mailbox user ID                    */

/* ******************************** ipc$o *********************************** */

/* ********** mode key **********/

/* k$read 1#define       Open mailbox for reading                  */

/* k$writ 2#define       Open mailbox for writing                  */

/* k$rdwr 3#define       Open mailbox for reading and writing      */

/* ********** notify key ********/

#define  k$nfin               1          /* Notify with interrupts for msg waiting    */
#define  k$nfsm               2          /* Notify semaphore for message waiting      */

/* ************************** ipc$r, ipc$ra ********************************* */

/* k$read 1#define       Read without waiting                      */

#define  k$rdwt               2          /* Read and wait if no data                  */

/* ******************************** ipc$st ********************************** */

#define  k$ipcb               2          /* First key for IPC$ST                      */

/* k$mmsg 1#define       Get maximum msgs per mailbox (obsolete)   */

#define  k$nmsg               2          /* Get number of msgs waiting for this user  */
#define  k$mrom               3          /* Get maximum space allowed for mailbox msgs*/
#define  k$room               4          /* Get remaining space available in mailbox  */
#define  k$nusr               5          /* Get number of users attached to mailbox   */
#define  k$nfys               6          /* Notify IPC$SEM database lock (for debug)  */
#define  k$ipce               6          /* Make this last for debug                  */


/* ******************************* chbk$$ ********************************** */

#define  k$awwt               2          /* wait for completion */
#define  k$save               4          /* retain error info */
#define  k$uri                8          /* check uri for any user */

/***************************** tty$rs **** ********************************* */

#define  k$outb               -32768     /* Clear output buffer                       */
#define  k$inb                16384      /* Clear input buffer                        */

/******************************** ds$asy *********************************** */

#define  k$lins               1          /* return summary of all async lines */
#define  k$line               2          /* return info for a specified async line */
#define  k$linu               3          /* return info for all async lines in use */
#define  k$linc               4          /* return info for all configured async lines */

/************************** sr$nextr & sr$exstr ******************************/

#define  k$bgn                268369920  /* binary value of null pointer */
#define  k$end                268369920  /* binary value of null pointer */
#define  k$text               1          /* the search rule is text.                  */
#define  k$hmdr               2          /* the rule is [HOME_DIR]                    */
#define  k$ordr               3          /* the rule is [ORIGIN_DIR]                  */
#define  k$rfdr               4          /* the rule is [REFERENCING_DIR]             */
#define  k$keyw               8          /* the rule is a keyword.                    */
#define  k$anyt               -1         /* find a rule regardless of its type.       */
#define  k$anytype            -1         /* find a rule regardless of its type.       */
#define  k$public_all         9          /* the rule is -PUBLIC                       */
#define  k$public_one         10         /* the rule is -PUBLIC <libname>             */
#define  k$public3_one        11         /* the rule is for ring 3                    */
#define  k$public0_one        12         /* the rule is for ring 0                    */

/************************** opsr$ & opsrs$ ***********************************/

#define  k$unkn               0          /* unknown type */
#define  k$acat               4096       /* access category */
#define  k$file               8192       /* file */
#define  k$sdir               16384      /* segment directory */
#define  k$dir                -32768     /* directory */
#define  k$nulf               256        /* for opsrs$, and srsfx$, to search for the */
                                             /* null suffix first */

/***************************** As$Set/As$Lst **********************************/

#define  k$sls                0          /* system login settings */
#define  k$plst               1          /* parameter list */
#define  k$gtal               2          /* get all parameters */

/***************************** Show$/Watch$ ***********************************/

#define  k$getf               0          /* get first user I am watching */
#define  k$getn               1          /* get next user I am watching  */
#define  k$gmyw               2          /* get user watching me         */
#define  k$setw               3          /* set watch access             */
#define  k$strt               4          /* start watch session          */
#define  k$stop               5          /* stop watch session           */

/******************************** cl$msg **************************************/

#define  k$long               0          /* long prompt indicator */
#define  k$brief              -32768     /* brief prompt indicator */

/***************************** bkp$op *********************************/

/*   k$bkup     7#define                    do not update dta when close     */
#define  k$obj                8          /* access and possibly open named object*/
#define  k$incr               16         /* incremental backup      */
#define  k$bufr               32         /* open for prwf$$ I/O (buffered I/O)   */
                                             /* the following keys are used during   */
                                             /* incr backups to indicate how to      */
                                             /* determine if an object should be     */
                                             /* opened (i.e. saved):                 */
#define  k$dbit               128        /* check the dumped bit                 */
#define  k$fdtb               256        /* compare the dtb and dtm              */
#define  k$udtb               512        /* cmp the dtm and a user supplied date */

/*********************** bkp$satr *********************************************/

/*   k$dmpb     3#define                    set the dumped bit                    */
/*   k$dtb      8#define                    set the dtb                           */
#define  k$unam               2048       /* set attributes on the named object    */
#define  k$uunt               4096       /* set attributes on the open object     */

/*******************************mm$share_apm *********************************/

#define  k$int0               0         /* segment initialized to zero    */
#define  k$data               1         /* reserved for future            */

/************************* disk tracking *************************************/

#define  k$cra_mismatch      1     /* cra mismatch detected on icop read */
#define  k$set_dta_dtm       2     /* bad set dta/dtm request */
#define  k$rtn_rec_error     3     /* error on rtn_rec attempt */

/************************* chprj$ and change_project *************************/

#define  k$chpr              0     /* Change project                     */
#define  k$encp              1     /* Enable CHPRJ$ and CHANGE_PROJECT   */
#define  k$dscp              2     /* Disable CHPRJ$ and CHANGE_PROJECT  */
#define  k$enpw              3     /* Enable passwords during CP         */
#define  k$dspw              4      /* Disable passwords during CP        */

/* End of insert file KEYS.INS.CC */
