#ifndef G_YS1YU96HRG7YT1C589YHKAGHG9KK8
#define G_YS1YU96HRG7YT1C589YHKAGHG9KK8
#include <stddef.h>

/* class Copyable a where
     copy : Ref a -> a
     destroy : a -> ()
 */
struct Ts_Copyable {
    void *(*copy)(const void *);
    void (*destroy)(void *);
};

/* data Function c a b = Function c ((c, a) -> b) */
struct Ts_Function {
    void *ctx;
    void *(*call)(void *, void *);
};

/* data Pair a b = Pair a b */
struct Ts_Pair {
    void *fst;
    void *snd;
};

/* data Result e a = Ok a | Err e */
struct Ts_Result {
    int which;
    union {
        void *ok;
        void *err;
    } value;
};

/* identity : a -> a */
void *ts_identity(void *x);

/* apply : (Function c a b, a) -> b */
void *ts_apply(struct Ts_Function f, void *x);

/* fst : Copyable b => Pair a b -> a */
void *ts_fst(struct Ts_Copyable i, struct Ts_Pair x);

/* ok : a -> Result e a */
struct Ts_Result ts_ok(void *x);

#endif
