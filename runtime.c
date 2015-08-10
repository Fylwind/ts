#include "runtime.h"

/* identity x = x */
void *ts_identity(void *x)
{
    void *r;
    r = x;
    return r;
}

/* apply (Function c f) x = f (c, x) */
void *ts_apply(struct Ts_Function f, void *x)
{
    void *r;
    r = f.call(f.ctx, x);
    return r;
}

/* fst (Pair x _) = x */
void *ts_fst(struct Ts_Copyable i, struct Ts_Pair x)
{
    void *r;
    r = x.fst;
    i.destroy(x.snd);
    return r;
}

/* ok x = Ok x */
struct Ts_Result ts_ok(void *x)
{
    struct Ts_Result r;
    r.which = 0;
    r.value.ok = x;
    return r;
}
