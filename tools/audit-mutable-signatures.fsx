// Lists public members whose signature exposes a mutable array, a ResizeArray or a Dictionary.
open System
open System.IO
open System.Reflection

let directory = fsi.CommandLineArgs[1]
let names = fsi.CommandLineArgs[2..]
AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ args ->
    let path = Path.Combine(directory, AssemblyName(args.Name).Name + ".dll")
    if File.Exists path then Assembly.LoadFrom path else null))
let rec mutableType (t: Type) =
    t.IsArray ||
    (t.IsGenericType && (let d = t.GetGenericTypeDefinition() in d = typedefof<ResizeArray<_>> || d = typedefof<Collections.Generic.Dictionary<_,_>> || d = typedefof<Collections.Generic.HashSet<_>>)) ||
    (t.IsGenericType && t.GetGenericArguments() |> Array.exists mutableType)
for name in names do
    let assembly = Assembly.LoadFrom(Path.Combine(directory, name + ".dll"))
    for t in assembly.GetExportedTypes() do
        for m in t.GetMethods(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly) do
            if not m.IsSpecialName && (mutableType m.ReturnType || m.GetParameters() |> Array.exists (fun p -> mutableType p.ParameterType)) then
                printfn "%s.%s : %s" t.FullName m.Name (m.ToString())
        for p in t.GetProperties(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly) do
            if mutableType p.PropertyType then printfn "%s.%s (property) : %s" t.FullName p.Name p.PropertyType.Name
