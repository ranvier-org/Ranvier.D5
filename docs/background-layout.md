# Owned background layouts

ReactiveLayout.create accepts a HostDispatcher, a reactive input snapshot reader,
and a cancellable pure kernel returning Result. Read State and LastGood from
normal Ranvier effects/memos. Retry explicitly reruns the current input.
Dispose the handle or its parent owner on the graph thread before host shutdown.

The reader must produce immutable data or owned copies. Workers must not access
signals, owners, DOM/native objects or shared mutable input. Check cancellation
inside bounded kernel chunks; the adapter cannot preempt a blocking kernel.

There is one running kernel and one replaceable pending input. Every request
advances its generation and cancels the running token. The next worker starts
only when the previous completion reaches the owner. Obsolete results never
publish. State/result publication is coalesced to one callback per host frame;
LastGood survives Running and Failed states. Kernel exceptions become typed
layout errors. Disposal cancels work and makes queued callbacks inert.

HostDispatcher.Post must be thread-safe and queue callbacks to the owner.
RequestFrame must queue them to a subsequent host frame. Neither runs inline.
The executable example adapter in examples/Shared/NativeDispatcher.fs uses
WebCoreHost.TryPost and RequestAnimationFrame. Keeping this adapter in the host
avoids forcing a native WebCore dependency into the reusable View package.

Race tests cover a worker completing after 100 newer requests, disposal before
completion, cooperative cancellation, fault/retry and last-good presentation.
Native acceptance tests exercise the real host queue and frame callbacks.
