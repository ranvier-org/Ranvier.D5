# Owned chart interaction

`Gesture.update` is pure; a host supplies events and executes returned commands.
`InputAdapter.attach` uses the SVG screen matrix for client coordinates, enters the
captured signal graph, and releases native captures and listeners on disposal.
Attach to the plot SVG/group whose coordinate system the chart uses. Give the
interactive element a tabindex when keyboard navigation is desired.

`ChartInteraction.attach` owns gesture and selection signals and an effect tracking
the chart's semantic IDs. Dispose the returned handle or its parent owner. Exited
IDs are removed from selection; changed plot bounds cancel active gestures.
Brush and lasso select hit-target centers. Hover exposes semantic IDs for tooltips;
arrow keys move focus, Enter/Space select, and Escape cancels and clears selection.
Commands let applications update their shared Ranvier signals for linked views.

Pinch uses two pointers; releasing either ends both captures so a pinch cannot
accidentally commit a rectangular or lasso selection. Lasso history is decimated
at the configured bound. Resize, focus loss and cancellation release captures.
The host must send resize notifications to a standalone input adapter.

The native acceptance executable exercises actual pointer capture, movement,
release and disposal outside graph callbacks on Windows, Linux and macOS.
The benchmark covers 10,000 lasso events with bounded storage.
