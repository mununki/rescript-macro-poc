module Ast = PpxlibRes.Ast
module Builder = PpxlibRes.Builder
module Deriver = PpxlibRes.Deriver

let firstTypeDecl = (typeDecls: array<Ast.typeDeclaration>) => Belt.Array.get(typeDecls, 0)

let accessorName = (fieldName: string) =>
  switch fieldName {
  | "name" => "getName"
  | other => "get" ++ other
  }

let spiceCodecName = (prefix: string, typeName: string) => prefix ++ String.capitalize(typeName)

@macro.deriver("accessors")
let deriveAccessors = (
  _ctx: Deriver.ctx,
  _args: option<Ast.payload>,
  typeDecls: array<Ast.typeDeclaration>,
): Deriver.result =>
  switch firstTypeDecl(typeDecls) {
  | None => {structure: [], signature: []}
  | Some(typeDecl) =>
    switch typeDecl.kind {
    | Ast.Ptype_record({fields}) =>
      let structure =
        fields->Belt.Array.map(field =>
          Builder.value(
            accessorName(field.name),
            Builder.fun_(
              Builder.varPat("value"),
              Builder.field(Builder.ident(["value"]), [field.name]),
            ),
          )
        )
      let signature =
        fields->Belt.Array.map(field =>
          Builder.valueSig(
            accessorName(field.name),
            Builder.arrowType(Builder.constrType([typeDecl.name]), field.typ),
          )
        )
      {structure, signature}
    | _ => {structure: [], signature: []}
    }
  }

@macro.deriver("spice")
let deriveSpice = (
  _ctx: Deriver.ctx,
  _args: option<Ast.payload>,
  typeDecls: array<Ast.typeDeclaration>,
): Deriver.result =>
  switch firstTypeDecl(typeDecls) {
  | None => {structure: [], signature: []}
  | Some(typeDecl) =>
    switch typeDecl.kind {
    | Ast.Ptype_record({fields}) =>
      let encodeName = spiceCodecName("encode", typeDecl.name)
      let decodeName = spiceCodecName("decode", typeDecl.name)
      {
        structure: [
          Builder.value(encodeName, Spice.encodeExpr(fields)),
          Builder.value(decodeName, Spice.decodeExpr(fields)),
        ],
        signature: [
          Builder.valueSig(
            encodeName,
            Builder.arrowType(
              Builder.constrType([typeDecl.name]),
              Builder.constrType(["JSON", "t"]),
            ),
          ),
          Builder.valueSig(
            decodeName,
            Builder.arrowType(
              Builder.constrType(["JSON", "t"]),
              Builder.constrType([typeDecl.name]),
            ),
          ),
        ],
      }
    | _ => {structure: [], signature: []}
    }
  }
