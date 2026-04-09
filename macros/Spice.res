module Ast = PpxlibRes.Ast
module Builder = PpxlibRes.Builder

type scalar =
  | String
  | Int
  | Float
  | Bool

let isSimpleType = (typ: Ast.coreType, name: string) =>
  switch typ.desc {
  | Ast.Ptyp_constr({path, args}) =>
    Belt.Array.length(path) == 1 &&
    Belt.Array.get(path, 0) == Some(name) &&
    Belt.Array.length(args) == 0
  | _ => false
  }

let fieldScalar = (field: Ast.labelDeclaration): scalar =>
  if isSimpleType(field.typ, "string") {
    String
  } else if isSimpleType(field.typ, "int") {
    Int
  } else if isSimpleType(field.typ, "float") {
    Float
  } else if isSimpleType(field.typ, "bool") {
    Bool
  } else {
    failwith(
      "spice deriver currently supports only record fields of type string, int, float, or bool: " ++
      field.name,
    )
  }

let jsonEncoderPath = (scalar: scalar) =>
  switch scalar {
  | String => ["JSON", "String"]
  | Int => ["JSON", "Number"]
  | Float => ["JSON", "Number"]
  | Bool => ["JSON", "Boolean"]
  }

let jsonDecoderPath = (scalar: scalar) =>
  switch scalar {
  | String => ["JSON", "Decode", "string"]
  | Int => ["JSON", "Decode", "float"]
  | Float => ["JSON", "Decode", "float"]
  | Bool => ["JSON", "Decode", "bool"]
  }

let encodedFieldValue = (field: Ast.labelDeclaration) => {
  let scalar = fieldScalar(field)
  let fieldExpr = Builder.field(Builder.ident(["value"]), [field.name])
  switch scalar {
  | Int =>
    Builder.construct(
      jsonEncoderPath(scalar),
      ~arg=Some(
        Builder.apply(Builder.ident(["Int", "toFloat"]), [Builder.arg(fieldExpr)]),
      ),
    )
  | _ => Builder.construct(jsonEncoderPath(scalar), ~arg=Some(fieldExpr))
  }
}

let decodeObjectExpr = (jsonExpr: Ast.expression) =>
  Builder.apply(
    Builder.ident(["Option", "getOrThrow"]),
    [
      Builder.arg(
        Builder.apply(Builder.ident(["JSON", "Decode", "object"]), [Builder.arg(jsonExpr)]),
      ),
    ],
  )

let decodedScalarValue = (expr: Ast.expression, scalar: scalar) =>
  switch scalar {
  | String | Float | Bool =>
    Builder.apply(Builder.ident(["Option", "getOrThrow"]), [Builder.arg(expr)])
  | Int =>
    Builder.apply(
      Builder.ident(["Int", "fromFloat"]),
      [
        Builder.arg(
          Builder.apply(Builder.ident(["Option", "getOrThrow"]), [Builder.arg(expr)]),
        ),
      ],
    )
  }

let decodeFieldValue = (field: Ast.labelDeclaration, objectExpr: Ast.expression) => {
  let scalar = fieldScalar(field)
  Builder.apply(
    Builder.ident(jsonDecoderPath(scalar)),
    [
      Builder.arg(
        Builder.apply(
          Builder.ident(["Dict", "getUnsafe"]),
          [Builder.arg(objectExpr), Builder.arg(Builder.string(field.name))],
        ),
      ),
    ],
  )->decodedScalarValue(scalar)
}

let encodeBody = (fields: array<Ast.labelDeclaration>) => {
  let objectPat = Builder.varPat("object_")
  let objectExpr = Builder.ident(["object_"])
  let objectBinding = Builder.valueBinding(
    objectPat,
    Builder.apply(Builder.ident(["Dict", "make"]), [Builder.arg(Builder.construct(["()"]))]),
  )
  let finishExpr = Builder.construct(["JSON", "Object"], ~arg=Some(objectExpr))
  let body =
    fields->Belt.Array.reduceReverse(finishExpr, (acc, field) =>
      Builder.seq(
        Builder.apply(
          Builder.ident(["Dict", "set"]),
          [
            Builder.arg(objectExpr),
            Builder.arg(Builder.string(field.name)),
            Builder.arg(encodedFieldValue(field)),
          ],
        ),
        acc,
      )
    )
  Builder.let_([objectBinding], body)
}

let decodeBody = (fields: array<Ast.labelDeclaration>) => {
  let objectPat = Builder.varPat("object_")
  let objectExpr = Builder.ident(["object_"])
  let objectBinding = Builder.valueBinding(objectPat, decodeObjectExpr(Builder.ident(["json"])))
  let body = Builder.record(
    fields->Belt.Array.map(field =>
      Builder.recordField(field.name, decodeFieldValue(field, objectExpr))
    ),
  )
  Builder.let_([objectBinding], body)
}

let encodeExpr = (fields: array<Ast.labelDeclaration>) =>
  Builder.fun_(Builder.varPat("value"), encodeBody(fields))

let decodeExpr = (fields: array<Ast.labelDeclaration>) =>
  Builder.fun_(Builder.varPat("json"), decodeBody(fields))
