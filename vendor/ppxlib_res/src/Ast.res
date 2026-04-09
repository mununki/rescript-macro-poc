type location = {
  file: option<string>,
  startLine: int,
  startCol: int,
  endLine: int,
  endCol: int,
  ghost: bool,
}

type longident = array<string>

type argLabel =
  | Nolabel
  | Labelled(string)
  | Optional(string)

type constant =
  | Pconst_integer({value: string, suffix: option<string>})
  | Pconst_char({value: int})
  | Pconst_string({value: string, delimiter: option<string>})
  | Pconst_float({value: string, suffix: option<string>})

type expIdent = {path: longident}

type expConstant = {constant: constant}

type patVar = {name: string}

type rec expression = {
  loc: option<location>,
  desc: expressionDesc,
}
and expressionDesc =
  | Pexp_ident(expIdent)
  | Pexp_constant(expConstant)
  | Pexp_apply(expApply)
  | Pexp_fun(expFun)
  | Pexp_construct(expConstruct)
  | Pexp_let(expLet)
  | Pexp_sequence(expSequence)
  | Pexp_record(expRecord)
  | Pexp_field(expField)
and expApply = {
  funct: expression,
  args: array<applyArg>,
}
and expFun = {
  argLabel: argLabel,
  default: option<expression>,
  lhs: pattern,
  rhs: expression,
  async: bool,
}
and expConstruct = {
  constructPath: longident,
  constructArg: option<expression>,
}
and expLet = {
  recursive: bool,
  bindings: array<valueBinding>,
  body: expression,
}
and expSequence = {
  first: expression,
  second: expression,
}
and expRecord = {
  fields: array<expressionField>,
  withExpr: option<expression>,
}
and expField = {
  exprTarget: expression,
  path: longident,
}
and pattern = {
  patternLoc: option<location>,
  patternDesc: patternDesc,
}
and patternDesc =
  | Ppat_any
  | Ppat_var(patVar)
  | Ppat_record(patRecord)
and patRecord = {
  patternFields: array<patternField>,
  closed: bool,
}
and expressionField = {
  lid: longident,
  expr: expression,
  optional: bool,
}
and patternField = {
  fieldLid: longident,
  pat: pattern,
  patternOptional: bool,
}
and applyArg = {
  label: argLabel,
  argExpr: expression,
}
and valueBinding = {
  bindingLoc: option<location>,
  bindingPat: pattern,
  bindingExpr: expression,
}

type rec coreType = {
  loc: option<location>,
  desc: coreTypeDesc,
}
and coreTypeDesc =
  | Ptyp_constr(typConstr)
  | Ptyp_arrow(typArrow)
  | Ptyp_tuple(typTuple)
and typConstr = {
  path: longident,
  args: array<coreType>,
}
and typArrow = {
  argLabel: argLabel,
  arg: coreType,
  ret: coreType,
}
and typTuple = {items: array<coreType>}

type valueDescription = {
  loc: option<location>,
  name: string,
  typ: coreType,
}

type labelDeclaration = {
  loc: option<location>,
  name: string,
  mutable_: bool,
  optional: bool,
  typ: coreType,
}

type constructorDeclaration = {
  loc: option<location>,
  name: string,
  args: array<coreType>,
}

type typeRecord = {fields: array<labelDeclaration>}

type typeVariant = {constructors: array<constructorDeclaration>}

type typeKind =
  | Ptype_abstract
  | Ptype_record(typeRecord)
  | Ptype_variant(typeVariant)

type typeDeclaration = {
  loc: option<location>,
  name: string,
  params: array<coreType>,
  manifest: option<coreType>,
  kind: typeKind,
}

type strEval = {expr: expression}

type strValue = {
  recursive: bool,
  bindings: array<valueBinding>,
}

type strType = {
  recursive: bool,
  declarations: array<typeDeclaration>,
}

type structureItemDesc =
  | Pstr_eval(strEval)
  | Pstr_value(strValue)
  | Pstr_type(strType)

type structureItem = {
  loc: option<location>,
  desc: structureItemDesc,
}

type sigValue = {value: valueDescription}

type sigType = {
  recursive: bool,
  declarations: array<typeDeclaration>,
}

type signatureItemDesc =
  | Psig_value(sigValue)
  | Psig_type(sigType)

type signatureItem = {
  loc: option<location>,
  desc: signatureItemDesc,
}

type payloadPat = {
  pat: pattern,
  guard: option<expression>,
}

type payload =
  | PStr(array<structureItem>)
  | PSig(array<signatureItem>)
  | PTyp(coreType)
  | PPat(payloadPat)
