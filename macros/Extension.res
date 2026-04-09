module Ast = PpxlibRes.Ast
module Builder = PpxlibRes.Builder
module Payload = PpxlibRes.Payload
module Extension = PpxlibRes.Extension

@macro.extension("graphql")
let expand = (
  kind: Extension.context,
  payload: Ast.payload,
  _ctx: Extension.ctx,
): Extension.result =>
  switch kind {
  | Pexp =>
    switch Payload.asSingleExpr(payload) {
    | Some(expr) =>
      Extension.Pexp(
        Builder.apply(
          Builder.ident(["++"]),
          [Builder.arg(Builder.string("query:")), Builder.arg(expr)],
        ),
      )
    | None => Extension.Pexp(Builder.string("query:"))
    }
  | Pstr =>
    switch Payload.asSingleExpr(payload) {
    | Some(expr) => Extension.Pstr([Builder.value("generatedValue", expr)])
    | None => Extension.Pstr([])
    }
  | Psig => Extension.Psig([Builder.valueSig("generatedValue", Builder.constrType(["string"]))])
  }
