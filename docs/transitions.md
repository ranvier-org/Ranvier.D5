# Keyed transitions

Use KeyedTransition.start/retarget/sample for pure state, or
KeyedTransition.animate with the existing host frame-clock signal, a mark-list
signal, an Anime Ease function, and the toolkit's reduced-motion policy.
The adapter owns memos, creates no timer, and needs no completion callbacks.

Compatible paths have matching command types/counts and arc flags. Coordinates,
text positions (unchanged text), affine coefficients, opacity, stroke width and
font size interpolate through Anime scalar interpolation. Paint or text changes
cross-fade; arbitrary CSS strings are never parsed. Easing output is clamped to
keep scene dimensions/radii/opacity valid. Affine interpolation is coefficient
interpolation, not rotation decomposition.

Interruption samples existing segments before retargeting. Exit and re-entry
share the domain key and cannot be removed by an old completion callback.
Reduced motion replaces the state immediately. Repeated incompatible changes
retain at most eight layers per key; the faintest old layers are discarded first.

Samples are SceneNode groups keyed by domain ID with private numeric leaf keys.
Compose them with Scene.compose. Use scoped renderer callbacks and the group's
domain key for semantic events; update semantic hit targets from the desired
chart, excluding exiting visual-only layers. Call one transition state per stable
plot/group; moving across parents follows normal View remount semantics.
