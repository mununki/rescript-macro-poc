let lid = (parts: array<string>): Ast.longident => parts

let noLabel = (): Ast.argLabel => Ast.Nolabel

let labelled = (name: string): Ast.argLabel => Ast.Labelled(name)

let optional = (name: string): Ast.argLabel => Ast.Optional(name)

let ident = (path: Ast.longident): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_ident({path: path}),
}

let string = (value: string): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_constant({
    constant: Ast.Pconst_string({value, delimiter: None}),
  }),
}

let int = (value: int): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_constant({
    constant: Ast.Pconst_integer({value: Int.toString(value), suffix: None}),
  }),
}

let construct = (path: Ast.longident, ~arg: option<Ast.expression>=None): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_construct({constructPath: path, constructArg: arg}),
}

let let_ = (
  bindings: array<Ast.valueBinding>,
  body: Ast.expression,
  ~recursive: bool=false,
): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_let({recursive, bindings, body}),
}

let seq = (first: Ast.expression, second: Ast.expression): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_sequence({first, second}),
}

let field = (expr: Ast.expression, path: Ast.longident): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_field({exprTarget: expr, path}),
}

let arg = (~label: Ast.argLabel=Ast.Nolabel, expr: Ast.expression): Ast.applyArg => {
  label,
  argExpr: expr,
}

let apply = (funct: Ast.expression, args: array<Ast.applyArg>): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_apply({funct, args}),
}

let fun_ = (
  ~argLabel: Ast.argLabel=Ast.Nolabel,
  ~default: option<Ast.expression>=None,
  ~async: bool=false,
  lhs: Ast.pattern,
  rhs: Ast.expression,
): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_fun({
    argLabel,
    default,
    lhs,
    rhs,
    async,
  }),
}

let recordField = (
  name: string,
  expr: Ast.expression,
  ~optional: bool=false,
): Ast.expressionField => {lid: [name], expr, optional}

let record = (
  fields: array<Ast.expressionField>,
  ~withExpr: option<Ast.expression>=None,
): Ast.expression => {
  loc: None,
  desc: Ast.Pexp_record({fields, withExpr}),
}

let anyPat = (): Ast.pattern => {patternLoc: None, patternDesc: Ast.Ppat_any}

let varPat = (name: string): Ast.pattern => {
  patternLoc: None,
  patternDesc: Ast.Ppat_var({name: name}),
}

let recordPatField = (name: string, pat: Ast.pattern, ~optional: bool=false): Ast.patternField => {
  fieldLid: [name],
  pat,
  patternOptional: optional,
}

let recordPat = (fields: array<Ast.patternField>, ~closed: bool=true): Ast.pattern => {
  patternLoc: None,
  patternDesc: Ast.Ppat_record({patternFields: fields, closed}),
}

let eval = (expr: Ast.expression): Ast.structureItem => {
  loc: None,
  desc: Ast.Pstr_eval({expr: expr}),
}

let valueBinding = (pat: Ast.pattern, expr: Ast.expression): Ast.valueBinding => {
  bindingLoc: None,
  bindingPat: pat,
  bindingExpr: expr,
}

let value = (name: string, expr: Ast.expression, ~recursive: bool=false): Ast.structureItem => {
  loc: None,
  desc: Ast.Pstr_value({
    recursive,
    bindings: [{bindingLoc: None, bindingPat: varPat(name), bindingExpr: expr}],
  }),
}

let valueDescription = (name: string, typ: Ast.coreType): Ast.valueDescription => {
  loc: None,
  name,
  typ,
}

let valueSig = (name: string, typ: Ast.coreType): Ast.signatureItem => {
  loc: None,
  desc: Ast.Psig_value({value: valueDescription(name, typ)}),
}

let constrType = (path: Ast.longident, ~args: array<Ast.coreType>=[]): Ast.coreType => {
  loc: None,
  desc: Ast.Ptyp_constr({path, args}),
}

let arrowType = (
  ~argLabel: Ast.argLabel=Ast.Nolabel,
  arg: Ast.coreType,
  ret: Ast.coreType,
): Ast.coreType => {
  loc: None,
  desc: Ast.Ptyp_arrow({argLabel, arg, ret}),
}

let tupleType = (items: array<Ast.coreType>): Ast.coreType => {
  loc: None,
  desc: Ast.Ptyp_tuple({items: items}),
}

let labelDecl = (
  name: string,
  typ: Ast.coreType,
  ~mutable_: bool=false,
  ~optional: bool=false,
): Ast.labelDeclaration => {
  loc: None,
  name,
  mutable_,
  optional,
  typ,
}

let tupleConstructor = (
  name: string,
  ~args: array<Ast.coreType>=[],
): Ast.constructorDeclaration => {loc: None, name, args}

let abstractType = (name: string): Ast.typeDeclaration => {
  loc: None,
  name,
  params: [],
  manifest: None,
  kind: Ast.Ptype_abstract,
}

let recordType = (name: string, fields: array<Ast.labelDeclaration>): Ast.typeDeclaration => {
  loc: None,
  name,
  params: [],
  manifest: None,
  kind: Ast.Ptype_record({fields: fields}),
}

let variantType = (
  name: string,
  constructors: array<Ast.constructorDeclaration>,
): Ast.typeDeclaration => {
  loc: None,
  name,
  params: [],
  manifest: None,
  kind: Ast.Ptype_variant({constructors: constructors}),
}
