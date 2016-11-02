-define(ETS_TAB, toveri_ring_buffer).

-record(ringbuf, {name :: atom(),
                  pos :: non_neg_integer(),
                  pid :: pid()}).
