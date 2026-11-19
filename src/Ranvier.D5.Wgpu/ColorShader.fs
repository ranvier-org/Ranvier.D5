namespace Ranvier.D5.Wgpu

open System
open System.Numerics
open Ranvier.Wgpu

/// <summary>GPU uniform scalar-LUT shader composition for zero-upload <c>colorrange</c> scrubbing via <c>WgpuContext.SetUserUniforms</c>.</summary>
[<RequireQualifiedAccess>]
module ColorShader =
    [<Literal>]
    let private MaxUniformStops = 13

    [<Literal>]
    let private WgslDeclarations =
        """fn d5_sample_ramp(scalar: f32) -> vec4<f32> {
    let cmin = user.slots[0].x;
    let cmax = user.slots[0].y;
    let span = cmax - cmin;
    var t: f32 = 0.5;
    if (abs(span) <= 1e-12) {
        if (scalar < cmin) {
            return user.slots[1];
        }
        if (scalar > cmax) {
            return user.slots[2];
        }
    } else {
        t = (scalar - cmin) / span;
        if (t < 0.0) {
            return user.slots[1];
        }
        if (t > 1.0) {
            return user.slots[2];
        }
    }
    if (!(t >= 0.0 && t <= 1.0)) {
        return user.slots[1];
    }
    let stopCount = clamp(i32(round(user.slots[0].z)), 1, 13);
    if (stopCount <= 1) {
        return user.slots[3];
    }
    let pos = clamp(t, 0.0, 1.0) * f32(stopCount - 1);
    let i0 = clamp(i32(floor(pos)), 0, stopCount - 1);
    let i1 = min(i0 + 1, stopCount - 1);
    let frac = pos - f32(i0);
    return mix(user.slots[3 + i0], user.slots[3 + i1], frac);
}"""

    [<Literal>]
    let private WgslVaryings = "    @location(5) d5Data: vec4<f32>,"

    [<Literal>]
    let private WgslVertexEnd =
        """    out.d5Data = vec4<f32>(in.vertexColor.x, in.colorRoughness.rgb);
    if (user.slots[0].w > 0.5) {
        let rampCol = d5_sample_ramp(in.vertexColor.x);
        out.colorRoughness = vec4<f32>(rampCol.rgb * in.colorRoughness.rgb, in.colorRoughness.a);
        out.opacityEmissive = vec2<f32>(rampCol.a * in.vertexColor.a * in.opacityEmissive.x, in.opacityEmissive.y);
    }"""

    [<Literal>]
    let private WgslFragmentLit =
        """    if (user.slots[0].w > 0.5 && ((flags & 1u) != 0u)) {
        let rampCol = d5_sample_ramp(in.d5Data.x);
        var unlitCol = rampCol.rgb * in.d5Data.yzw * (1.0 + in.opacityEmissive.y);
        if ((flags & 2u) != 0u) {
            unlitCol = mix(unlitCol, vec3<f32>(0.45, 0.96, 1.0), 0.45) + vec3<f32>(0.15, 0.35, 0.45);
        }
        lit = unlitCol;
    }"""

    /// <summary>
    /// Composes the custom WGSL scene shader sampling <c>user.slots[3..15]</c> whenever <c>user.slots[0].w &gt; 0.5</c>.
    /// Note that <c>user.slots[0].w &gt; 0.5</c> remaps vertex colors across all <c>InstanceBatch</c>es rendered in that <c>WgpuContext</c> pass
    /// (including <c>Axis3D</c> panels unless disabled with <c>ColorShader.disabledUniforms ()</c> or drawn in a separate context/uniform state).
    /// <c>ShadingMode.Lit</c> interpolates vertex-sampled LUT RGB across triangles, whereas <c>ShadingMode.Unlit</c> samples <c>d5_sample_ramp</c> per-fragment.
    /// </summary>
    let wgsl () : string =
        Shaders.composeScene
            [
                SceneShaderHook.Varyings, WgslVaryings
                SceneShaderHook.Declarations, WgslDeclarations
                SceneShaderHook.VertexEnd, WgslVertexEnd
                SceneShaderHook.FragmentLit, WgslFragmentLit
            ]

#if FABLE_COMPILER
    [<Literal>]
    let private GlslDeclarations =
        """vec4 d5_sample_ramp(float scalar) {
    float cmin = user.slots[0].x;
    float cmax = user.slots[0].y;
    float span = cmax - cmin;
    float t = 0.5;
    if (abs(span) <= 1e-12) {
        if (scalar < cmin) {
            return user.slots[1];
        }
        if (scalar > cmax) {
            return user.slots[2];
        }
    } else {
        t = (scalar - cmin) / span;
        if (t < 0.0) {
            return user.slots[1];
        }
        if (t > 1.0) {
            return user.slots[2];
        }
    }
    if (!(t >= 0.0 && t <= 1.0)) {
        return user.slots[1];
    }
    int stopCount = clamp(int(round(user.slots[0].z)), 1, 13);
    if (stopCount <= 1) {
        return user.slots[3];
    }
    float pos = clamp(t, 0.0, 1.0) * float(stopCount - 1);
    int i0 = clamp(int(floor(pos)), 0, stopCount - 1);
    int i1 = min(i0 + 1, stopCount - 1);
    float frac = pos - float(i0);
    return mix(user.slots[3 + i0], user.slots[3 + i1], frac);
}"""

    [<Literal>]
    let private GlslVaryings = "VARYING vec4 v_d5Data;"

    [<Literal>]
    let private GlslVertexEnd =
        """    v_d5Data = vec4(a_vertexColor.x, a_colorRoughness.rgb);
    if (user.slots[0].w > 0.5) {
        vec4 rampCol = d5_sample_ramp(a_vertexColor.x);
        v_colorRoughness = vec4(rampCol.rgb * a_colorRoughness.rgb, a_colorRoughness.a);
        v_opacityEmissive = vec2(rampCol.a * a_vertexColor.a * a_opacityEmissive.x, a_opacityEmissive.y);
    }"""

    [<Literal>]
    let private GlslFragmentLit =
        """    if (user.slots[0].w > 0.5 && ((flags & 1u) != 0u)) {
        vec4 rampCol = d5_sample_ramp(v_d5Data.x);
        vec3 unlitCol = rampCol.rgb * v_d5Data.yzw * (1.0 + v_opacityEmissive.y);
        if ((flags & 2u) != 0u) {
            unlitCol = mix(unlitCol, vec3(0.45, 0.96, 1.0), 0.45) + vec3(0.15, 0.35, 0.45);
        }
        lit = unlitCol;
    }"""

    /// <summary>
    /// Composes the WebGL2 GLSL ES 3.00 scene shader matching <c>ColorShader.wgsl ()</c>.
    /// Note that <c>user.slots[0].w &gt; 0.5</c> remaps vertex colors across all <c>InstanceBatch</c>es rendered in that <c>WgpuContext</c> pass
    /// (including <c>Axis3D</c> panels unless disabled with <c>ColorShader.disabledUniforms ()</c> or drawn in a separate context/uniform state).
    /// <c>ShadingMode.Lit</c> interpolates vertex-sampled LUT RGB across triangles, whereas <c>ShadingMode.Unlit</c> samples <c>d5_sample_ramp</c> per-fragment.
    /// </summary>
    let glsl () : string =
        GlslShaders.composeScene
            [
                SceneShaderHook.Varyings, GlslVaryings
                SceneShaderHook.Declarations, GlslDeclarations
                SceneShaderHook.VertexEnd, GlslVertexEnd
                SceneShaderHook.FragmentLit, GlslFragmentLit
            ]
#endif

    /// <summary>
    /// Packs <c>[cmin, cmax, 13, 1]</c>, clip colors, and <c>13</c> ramp stops into <c>16</c> <c>Vector4</c> user uniform slots.
    /// Applying these uniforms enables vertex-color LUT remapping (<c>user.slots[0].w &gt; 0.5</c>) across all <c>InstanceBatch</c>es in that
    /// <c>WgpuContext</c> pass (including <c>Axis3D</c> panels unless reset via <c>ColorShader.disabledUniforms ()</c> or drawn in a separate context/uniform state);
    /// <c>ShadingMode.Lit</c> samples per-vertex while <c>ShadingMode.Unlit</c> samples per-fragment.
    /// </summary>
    let packUniforms (cmin: float32) (cmax: float32) (ramp: ColorRamp) : ReadOnlyMemory<Vector4> =
        Check3D.finite32 (nameof cmin) cmin
        Check3D.finite32 (nameof cmax) cmax

        if isNull (box ramp) then
            nullArg (nameof ramp)

        let slots = Array.zeroCreate<Vector4> Shaders.UserUniformSlots
        slots[0] <- Vector4 (cmin, cmax, float32 MaxUniformStops, 1.0f)

        let lowColor =
            match ramp.ClipPolicy.LowClip with
            | ValueSome c -> c
            | ValueNone -> ramp.LowStop

        let highColor =
            match ramp.ClipPolicy.HighClip with
            | ValueSome c -> c
            | ValueNone -> ramp.HighStop

        slots[1] <- lowColor.ToVector4 ()
        slots[2] <- highColor.ToVector4 ()

        let stops = ColorRamp.stops MaxUniformStops ramp
        let stopSpan = stops.Span

        for i = 0 to MaxUniformStops - 1 do
            slots[3 + i] <- stopSpan[i].ToVector4 ()

        ReadOnlyMemory<Vector4>(slots)

    /// <summary>Returns a <c>16</c>-element zeroed <c>ReadOnlyMemory&lt;Vector4&gt;</c> (<c>user.slots[0].w = 0.0</c>) disabling <c>ColorShader</c> LUT remapping.</summary>
    let disabledUniforms () : ReadOnlyMemory<Vector4> =
        ReadOnlyMemory<Vector4>(Array.zeroCreate<Vector4> Shaders.UserUniformSlots)
