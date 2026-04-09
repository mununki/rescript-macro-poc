type configField = {
  name: string,
  expr: Ast.expression,
}

let lastSegment = (path: Ast.longident) =>
  if Belt.Array.length(path) === 0 {
    ""
  } else {
    Belt.Array.getUnsafe(path, Belt.Array.length(path) - 1)
  }

let isEmpty = (payload: Ast.payload): bool =>
  switch payload {
  | Ast.PStr(items) => Belt.Array.length(items) === 0
  | Ast.PSig(items) => Belt.Array.length(items) === 0
  | _ => false
  }

let asSingleExpr = (payload: Ast.payload): option<Ast.expression> =>
  switch payload {
  | Ast.PStr([{desc: Ast.Pstr_eval({expr})}]) => Some(expr)
  | _ => None
  }

let asConfigRecord = (payload: Ast.payload): option<array<configField>> =>
  switch asSingleExpr(payload) {
  | Some({desc: Ast.Pexp_record({fields, withExpr: None})}) =>
    Some(fields->Belt.Array.map(field => {name: lastSegment(field.lid), expr: field.expr}))
  | _ => None
  }
