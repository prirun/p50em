typedef struct {
  char          m_bClear;
}  stopwatch_t;

static stopwatch_t sw_all, sw_fault, sw_ea, sw_mapva, sw_idle, sw_add16, sw_cas, sw_irs, sw_io, sw_zmv, sw_zmvd, sw_zfil, sw_pcl;

#define stopwatch_init(X,Y)
#define stopwatch_start(X)
#define stopwatch_stop(X)
#define stopwatch_report(X)
#define stopwatch_push(X)
#define stopwatch_pop(X)
