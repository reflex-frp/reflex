#include <Rts.h>

// The InCall structure represents either a single in-call from C to
// Haskell, or a worker thread.
typedef struct InCall_ {
    StgTSO *   tso;             // the bound TSO (or NULL for a worker)

    StgTSO *   suspended_tso;   // the TSO is stashed here when we
                                // make a foreign call (NULL otherwise);

    Capability *suspended_cap;  // The capability that the
                                // suspended_tso is on, because
                                // we can't read this from the TSO
                                // without owning a Capability in the
                                // first place.

    SchedulerStatus  rstat;     // return status
    StgClosure **    ret;       // return value

    struct Task_ *task;

    // When a Haskell thread makes a foreign call that re-enters
    // Haskell, we end up with another Task associated with the
    // current thread.  We have to remember the whole stack of InCalls
    // associated with the current Task so that we can correctly
    // save & restore the InCall on entry to and exit from Haskell.
    struct InCall_ *prev_stack;

    // Links InCalls onto suspended_ccalls, spare_incalls
    struct InCall_ *prev;
    struct InCall_ *next;
} InCall;

typedef struct Task_ {
#if defined(THREADED_RTS)
    OSThreadId id;              // The OS Thread ID of this task

    Condition cond;             // used for sleeping & waking up this task
    Mutex lock;                 // lock for the condition variable

    // this flag tells the task whether it should wait on task->cond
    // or just continue immediately.  It's a workaround for the fact
    // that signalling a condition variable doesn't do anything if the
    // thread is already running, but we want it to be sticky.
    rtsBool wakeup;
#endif

    // This points to the Capability that the Task "belongs" to.  If
    // the Task owns a Capability, then task->cap points to it.  If
    // the task does not own a Capability, then either (a) if the task
    // is a worker, then task->cap points to the Capability it belongs
    // to, or (b) it is returning from a foreign call, then task->cap
    // points to the Capability with the returning_worker queue that this
    // this Task is on.
    //
    // When a task goes to sleep, it may be migrated to a different
    // Capability.  Hence, we always check task->cap on wakeup.  To
    // syncrhonise between the migrater and the migratee, task->lock
    // must be held when modifying task->cap.
    struct Capability_ *cap;

    // The current top-of-stack InCall
    struct InCall_ *incall;

    nat n_spare_incalls;
    struct InCall_ *spare_incalls;

    rtsBool    worker;          // == rtsTrue if this is a worker Task
    rtsBool    stopped;         // this task has stopped or exited Haskell

    // So that we can detect when a finalizer illegally calls back into Haskell
    rtsBool running_finalizers;

    // Links tasks on the returning_tasks queue of a Capability, and
    // on spare_workers.
    struct Task_ *next;

    // Links tasks on the all_tasks list; need ACQUIRE_LOCK(&all_tasks_mutex)
    struct Task_ *all_next;
    struct Task_ *all_prev;

} Task;

struct Capability_ {
    // State required by the STG virtual machine when running Haskell
    // code.  During STG execution, the BaseReg register always points
    // to the StgRegTable of the current Capability (&cap->r).
    StgFunTable f;
    StgRegTable r;

    nat no;  // capability number.

    // The Task currently holding this Capability.  This task has
    // exclusive access to the contents of this Capability (apart from
    // returning_tasks_hd/returning_tasks_tl).
    // Locks required: cap->lock.
    Task *running_task;

    // true if this Capability is running Haskell code, used for
    // catching unsafe call-ins.
    rtsBool in_haskell;

    // Has there been any activity on this Capability since the last GC?
    nat idle;

    rtsBool disabled;

    // The run queue.  The Task owning this Capability has exclusive
    // access to its run queue, so can wake up threads without
    // taking a lock, and the common path through the scheduler is
    // also lock-free.
    StgTSO *run_queue_hd;
    StgTSO *run_queue_tl;

    // Tasks currently making safe foreign calls.  Doubly-linked.
    // When returning, a task first acquires the Capability before
    // removing itself from this list, so that the GC can find all
    // the suspended TSOs easily.  Hence, when migrating a Task from
    // the returning_tasks list, we must also migrate its entry from
    // this list.
    InCall *suspended_ccalls;

    // One mutable list per generation, so we don't need to take any
    // locks when updating an old-generation thunk.  This also lets us
    // keep track of which closures this CPU has been mutating, so we
    // can traverse them using the right thread during GC and avoid
    // unnecessarily moving the data from one cache to another.
    bdescr **mut_lists;
    bdescr **saved_mut_lists; // tmp use during GC

    // block for allocating pinned objects into
    bdescr *pinned_object_block;
    // full pinned object blocks allocated since the last GC
    bdescr *pinned_object_blocks;

    // per-capability weak pointer list associated with nursery (older
    // lists stored in generation object)
    StgWeak *weak_ptr_list_hd;
    StgWeak *weak_ptr_list_tl;

    // Context switch flag.  When non-zero, this means: stop running
    // Haskell code, and switch threads.
    int context_switch;

    // Interrupt flag.  Like the context_switch flag, this also
    // indicates that we should stop running Haskell code, but we do
    // *not* switch threads.  This is used to stop a Capability in
    // order to do GC, for example.
    //
    // The interrupt flag is always reset before we start running
    // Haskell code, unlike the context_switch flag which is only
    // reset after we have executed the context switch.
    int interrupt;

    // Total words allocated by this cap since rts start
    // See [Note allocation accounting] in Storage.c
    W_ total_allocated;

#if defined(THREADED_RTS)
    // Worker Tasks waiting in the wings.  Singly-linked.
    Task *spare_workers;
    nat n_spare_workers; // count of above

    // This lock protects:
    //    running_task
    //    returning_tasks_{hd,tl}
    //    wakeup_queue
    //    inbox
    Mutex lock;

    // Tasks waiting to return from a foreign call, or waiting to make
    // a new call-in using this Capability (NULL if empty).
    // NB. this field needs to be modified by tasks other than the
    // running_task, so it requires cap->lock to modify.  A task can
    // check whether it is NULL without taking the lock, however.
    Task *returning_tasks_hd; // Singly-linked, with head/tail
    Task *returning_tasks_tl;

    // Messages, or END_TSO_QUEUE.
    // Locks required: cap->lock
    Message *inbox;

    SparkPool *sparks;

    // Stats on spark creation/conversion
    SparkCounters spark_stats;
#if !defined(mingw32_HOST_OS)
    // IO manager for this cap
    int io_manager_control_wr_fd;
#endif
#endif

    // Per-capability STM-related data
    StgTVarWatchQueue *free_tvar_watch_queues;
    StgInvariantCheckQueue *free_invariant_check_queues;
    StgTRecChunk *free_trec_chunks;
    StgTRecHeader *free_trec_headers;
    nat transaction_tokens;
} // typedef Capability is defined in RtsAPI.h
  // We never want a Capability to overlap a cache line with anything
  // else, so round it up to a cache line size:
#ifndef mingw32_HOST_OS
  ATTRIBUTE_ALIGNED(64)
#endif
  ;

HsBool myCapabilityHasOtherRunnableThreads() {
  return rts_unsafeGetMyCapability()->run_queue_hd == END_TSO_QUEUE ? HS_BOOL_FALSE : HS_BOOL_TRUE;
}
