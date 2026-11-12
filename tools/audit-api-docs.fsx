// Lists public types and members that have no XML documentation entry, per assembly.
open System
open System.IO
open System.Reflection
open System.Xml.Linq

let directory = fsi.CommandLineArgs[1]
let names = fsi.CommandLineArgs[2..]
AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ args ->
    let path = Path.Combine(directory, AssemblyName(args.Name).Name + ".dll")
    if File.Exists path then Assembly.LoadFrom path else null))

for name in names do
    let assembly = Assembly.LoadFrom(Path.Combine(directory, name + ".dll"))
    let xml = XDocument.Load(Path.Combine(directory, name + ".xml"))
    let documented =
        xml.Descendants(XName.Get "member")
        |> Seq.filter (fun m -> m.Element(XName.Get "summary") <> null && m.Element(XName.Get "summary").Value.Trim() <> "")
        |> Seq.map (fun m -> m.Attribute(XName.Get "name").Value)
        |> Set.ofSeq
    let prefixMatch (prefix: string) = documented |> Seq.exists (fun id -> id = prefix || id.StartsWith(prefix + "(") || id.StartsWith(prefix + "``"))
    let missing = ResizeArray<string>()
    for t in assembly.GetExportedTypes() do
        let typeName = t.FullName.Replace('+', '.')
        let isModule = t.GetCustomAttributes() |> Seq.exists (fun a -> a.GetType().Name = "CompilationMappingAttribute" && (string (a.GetType().GetProperty("SourceConstructFlags").GetValue a)).Contains "Module")
        let outer = if t.IsNested then t.DeclaringType.FullName.Replace('+', '.') else ""
        let unionCase = t.IsNested && (t.Name = "Tags" || prefixMatch ("P:" + outer + "." + t.Name) || prefixMatch ("M:" + outer + ".New" + t.Name) || documented.Contains("T:" + outer))
        if not (documented.Contains("T:" + typeName)) && not unionCase then
            missing.Add("T:" + typeName)
        if isModule then
            for m in t.GetMethods(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly) do
                if not m.IsSpecialName && not (prefixMatch ("M:" + typeName + "." + m.Name)) then missing.Add("M:" + typeName + "." + m.Name)
            for p in t.GetProperties(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly) do
                if not (prefixMatch ("P:" + typeName + "." + p.Name)) && not (prefixMatch ("M:" + typeName + "." + p.Name)) then missing.Add("P:" + typeName + "." + p.Name)
    let members = missing |> Seq.distinct |> Seq.toList
    printfn "%s: %d undocumented public types or module members" name members.Length
    for m in members do printfn "  %s" m
