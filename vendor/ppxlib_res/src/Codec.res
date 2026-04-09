type raw

external rawToString: raw => string = "%identity"
external rawToInt: raw => int = "%identity"
external rawToBool: raw => bool = "%identity"
external rawToArray: raw => array<raw> = "%identity"
external rawToOption: raw => option<raw> = "%identity"
external rawToDict: raw => dict<raw> = "%identity"
external stringToRaw: string => raw = "%identity"
external intToRaw: int => raw = "%identity"
external boolToRaw: bool => raw = "%identity"
external arrayToRaw: array<raw> => raw = "%identity"
external optionToRaw: option<raw> => raw = "%identity"
external dictToRaw: dict<raw> => raw = "%identity"
external castPayload: raw => Ast.payload = "%identity"
external castTypeDecls: raw => array<Ast.typeDeclaration> = "%identity"
external castExtensionContext: raw => Extension.context = "%identity"
external castExtensionResult: Extension.result => raw = "%identity"
external castDeriverResult: Deriver.result => raw = "%identity"

let get = (value: raw, key: string) => Dict.get(value->rawToDict, key)

let raiseDecodeError = (message: string) => JsError.throwWithMessage(message)

let getExn = (value: raw, key: string) =>
  switch get(value, key) {
  | Some(value) => value
  | None => raiseDecodeError("Missing field " ++ key)
  }

let decodeOption = (f, value: option<raw>) =>
  switch value {
  | None => None
  | Some(value) => Some(f(value))
  }

let decodeLocation = (value: raw): Ast.location => {
  file: decodeOption(rawToString, get(value, "file")),
  startLine: getExn(value, "startLine")->rawToInt,
  startCol: getExn(value, "startCol")->rawToInt,
  endLine: getExn(value, "endLine")->rawToInt,
  endCol: getExn(value, "endCol")->rawToInt,
  ghost: switch get(value, "ghost") {
  | Some(value) => value->rawToBool
  | None => false
  },
}

let decodeArgLabel = (value: raw): Ast.argLabel =>
  switch getExn(value, "kind")->rawToString {
  | "nolabel" => Ast.Nolabel
  | "labelled" => Ast.Labelled(getExn(value, "name")->rawToString)
  | "optional" => Ast.Optional(getExn(value, "name")->rawToString)
  | other => raiseDecodeError("Unsupported argLabel kind: " ++ other)
  }

let decodeConstant = (value: raw): Ast.constant =>
  switch getExn(value, "kind")->rawToString {
  | "integer" =>
    Ast.Pconst_integer({
      value: getExn(value, "value")->rawToString,
      suffix: decodeOption(rawToString, get(value, "suffix")),
    })
  | "char" => Ast.Pconst_char({value: getExn(value, "value")->rawToInt})
  | "string" =>
    Ast.Pconst_string({
      value: getExn(value, "value")->rawToString,
      delimiter: decodeOption(rawToString, get(value, "delimiter")),
    })
  | "float" =>
    Ast.Pconst_float({
      value: getExn(value, "value")->rawToString,
      suffix: decodeOption(rawToString, get(value, "suffix")),
    })
  | other => raiseDecodeError("Unsupported constant kind: " ++ other)
  }

let rec decodeExpression = (value: raw): Ast.expression => {
  let loc = decodeOption(decodeLocation, get(value, "loc"))
  let descValue = getExn(value, "desc")
  let desc = switch getExn(descValue, "kind")->rawToString {
  | "ident" =>
    Ast.Pexp_ident({path: getExn(descValue, "path")->rawToArray->Belt.Array.map(rawToString)})
  | "constant" => Ast.Pexp_constant({constant: decodeConstant(getExn(descValue, "constant"))})
  | "apply" =>
    Ast.Pexp_apply({
      funct: decodeExpression(getExn(descValue, "funct")),
      args: getExn(descValue, "args")->rawToArray->Belt.Array.map(decodeApplyArg),
    })
  | "fun" =>
    Ast.Pexp_fun({
      argLabel: decodeArgLabel(getExn(descValue, "argLabel")),
      default: decodeOption(decodeExpression, get(descValue, "default")),
      lhs: decodePattern(getExn(descValue, "lhs")),
      rhs: decodeExpression(getExn(descValue, "rhs")),
      async: switch get(descValue, "async") {
      | Some(value) => value->rawToBool
      | None => false
      },
    })
  | "construct" =>
    Ast.Pexp_construct({
      constructPath: getExn(descValue, "path")->rawToArray->Belt.Array.map(rawToString),
      constructArg: decodeOption(decodeExpression, get(descValue, "arg")),
    })
  | "let" =>
    Ast.Pexp_let({
      recursive: switch get(descValue, "recursive") {
      | Some(value) => value->rawToBool
      | None => false
      },
      bindings: getExn(descValue, "bindings")->rawToArray->Belt.Array.map(decodeValueBinding),
      body: decodeExpression(getExn(descValue, "body")),
    })
  | "sequence" =>
    Ast.Pexp_sequence({
      first: decodeExpression(getExn(descValue, "first")),
      second: decodeExpression(getExn(descValue, "second")),
    })
  | "record" =>
    Ast.Pexp_record({
      fields: getExn(descValue, "fields")->rawToArray->Belt.Array.map(decodeExpressionField),
      withExpr: decodeOption(decodeExpression, get(descValue, "withExpr")),
    })
  | "field" =>
    Ast.Pexp_field({
      exprTarget: decodeExpression(getExn(descValue, "expr")),
      path: getExn(descValue, "path")->rawToArray->Belt.Array.map(rawToString),
    })
  | other => raiseDecodeError("Unsupported expression kind: " ++ other)
  }
  {loc, desc}
}
and decodePattern = (value: raw): Ast.pattern => {
  let loc = decodeOption(decodeLocation, get(value, "loc"))
  let descValue = getExn(value, "desc")
  let desc = switch getExn(descValue, "kind")->rawToString {
  | "any" => Ast.Ppat_any
  | "var" => Ast.Ppat_var({name: getExn(descValue, "name")->rawToString})
  | "record" =>
    Ast.Ppat_record({
      patternFields: getExn(descValue, "fields")->rawToArray->Belt.Array.map(decodePatternField),
      closed: switch get(descValue, "closed") {
      | Some(value) => value->rawToBool
      | None => true
      },
    })
  | other => raiseDecodeError("Unsupported pattern kind: " ++ other)
  }
  {patternLoc: loc, patternDesc: desc}
}
and decodeCoreType = (value: raw): Ast.coreType => {
  let loc = decodeOption(decodeLocation, get(value, "loc"))
  let descValue = getExn(value, "desc")
  let desc = switch getExn(descValue, "kind")->rawToString {
  | "constr" =>
    Ast.Ptyp_constr({
      path: getExn(descValue, "path")->rawToArray->Belt.Array.map(rawToString),
      args: getExn(descValue, "args")->rawToArray->Belt.Array.map(decodeCoreType),
    })
  | "arrow" =>
    Ast.Ptyp_arrow({
      argLabel: decodeArgLabel(getExn(descValue, "argLabel")),
      arg: decodeCoreType(getExn(descValue, "arg")),
      ret: decodeCoreType(getExn(descValue, "ret")),
    })
  | "tuple" =>
    Ast.Ptyp_tuple({
      items: getExn(descValue, "items")->rawToArray->Belt.Array.map(decodeCoreType),
    })
  | other => raiseDecodeError("Unsupported coreType kind: " ++ other)
  }
  {loc, desc}
}
and decodeApplyArg = (value: raw): Ast.applyArg => {
  let arg: Ast.applyArg = {
    label: decodeArgLabel(getExn(value, "label")),
    argExpr: decodeExpression(getExn(value, "expr")),
  }
  arg
}
and decodeExpressionField = (value: raw): Ast.expressionField => {
  let field: Ast.expressionField = {
    lid: getExn(value, "lid")->rawToArray->Belt.Array.map(rawToString),
    expr: decodeExpression(getExn(value, "expr")),
    optional: switch get(value, "optional") {
    | Some(value) => value->rawToBool
    | None => false
    },
  }
  field
}
and decodePatternField = (value: raw): Ast.patternField => {
  let field: Ast.patternField = {
    fieldLid: getExn(value, "lid")->rawToArray->Belt.Array.map(rawToString),
    pat: decodePattern(getExn(value, "pat")),
    patternOptional: switch get(value, "optional") {
    | Some(value) => value->rawToBool
    | None => false
    },
  }
  field
}
and decodeValueBinding = (value: raw): Ast.valueBinding => {
  bindingLoc: decodeOption(decodeLocation, get(value, "loc")),
  bindingPat: decodePattern(getExn(value, "pat")),
  bindingExpr: decodeExpression(getExn(value, "expr")),
}
and decodeValueDescription = (value: raw): Ast.valueDescription => {
  loc: decodeOption(decodeLocation, get(value, "loc")),
  name: getExn(value, "name")->rawToString,
  typ: decodeCoreType(getExn(value, "typ")),
}
and decodeLabelDeclaration = (value: raw): Ast.labelDeclaration => {
  loc: decodeOption(decodeLocation, get(value, "loc")),
  name: getExn(value, "name")->rawToString,
  mutable_: switch get(value, "mutable") {
  | Some(value) => value->rawToBool
  | None => false
  },
  optional: switch get(value, "optional") {
  | Some(value) => value->rawToBool
  | None => false
  },
  typ: decodeCoreType(getExn(value, "typ")),
}
and decodeConstructorDeclaration = (value: raw): Ast.constructorDeclaration => {
  loc: decodeOption(decodeLocation, get(value, "loc")),
  name: getExn(value, "name")->rawToString,
  args: getExn(value, "args")->rawToArray->Belt.Array.map(decodeCoreType),
}
and decodeTypeKind = (value: raw): Ast.typeKind =>
  switch getExn(value, "kind")->rawToString {
  | "abstract" => Ast.Ptype_abstract
  | "record" =>
    Ast.Ptype_record({
      fields: getExn(value, "fields")->rawToArray->Belt.Array.map(decodeLabelDeclaration),
    })
  | "variant" =>
    Ast.Ptype_variant({
      constructors: getExn(value, "constructors")
      ->rawToArray
      ->Belt.Array.map(decodeConstructorDeclaration),
    })
  | other => raiseDecodeError("Unsupported type kind: " ++ other)
  }
and decodeTypeDeclaration = (value: raw): Ast.typeDeclaration => {
  loc: decodeOption(decodeLocation, get(value, "loc")),
  name: getExn(value, "name")->rawToString,
  params: getExn(value, "params")->rawToArray->Belt.Array.map(decodeCoreType),
  manifest: decodeOption(decodeCoreType, get(value, "manifest")),
  kind: decodeTypeKind(getExn(value, "kind")),
}
and decodeStructureItem = (value: raw): Ast.structureItem => {
  let loc = decodeOption(decodeLocation, get(value, "loc"))
  let descValue = getExn(value, "desc")
  let desc = switch getExn(descValue, "kind")->rawToString {
  | "eval" => Ast.Pstr_eval({expr: decodeExpression(getExn(descValue, "expr"))})
  | "value" =>
    Ast.Pstr_value({
      recursive: switch get(descValue, "recursive") {
      | Some(value) => value->rawToBool
      | None => false
      },
      bindings: getExn(descValue, "bindings")->rawToArray->Belt.Array.map(decodeValueBinding),
    })
  | "type" =>
    Ast.Pstr_type({
      recursive: switch get(descValue, "recursive") {
      | Some(value) => value->rawToBool
      | None => false
      },
      declarations: getExn(descValue, "declarations")
      ->rawToArray
      ->Belt.Array.map(decodeTypeDeclaration),
    })
  | other => raiseDecodeError("Unsupported structure item kind: " ++ other)
  }
  {loc, desc}
}
and decodeSignatureItem = (value: raw): Ast.signatureItem => {
  let loc = decodeOption(decodeLocation, get(value, "loc"))
  let descValue = getExn(value, "desc")
  let desc = switch getExn(descValue, "kind")->rawToString {
  | "value" => Ast.Psig_value({value: decodeValueDescription(getExn(descValue, "value"))})
  | "type" =>
    Ast.Psig_type({
      recursive: switch get(descValue, "recursive") {
      | Some(value) => value->rawToBool
      | None => false
      },
      declarations: getExn(descValue, "declarations")
      ->rawToArray
      ->Belt.Array.map(decodeTypeDeclaration),
    })
  | other => raiseDecodeError("Unsupported signature item kind: " ++ other)
  }
  {loc, desc}
}

let decodeExtensionContext = (value: raw): Extension.context =>
  switch value->rawToString {
  | "expression" => Extension.Pexp
  | "structure" => Extension.Pstr
  | "signature" => Extension.Psig
  | other => raiseDecodeError("Unsupported extension context: " ++ other)
  }

let decodePayload = (value: raw): Ast.payload =>
  switch getExn(value, "kind")->rawToString {
  | "structure" => Ast.PStr(getExn(value, "items")->rawToArray->Belt.Array.map(decodeStructureItem))
  | "signature" => Ast.PSig(getExn(value, "items")->rawToArray->Belt.Array.map(decodeSignatureItem))
  | "type" => Ast.PTyp(decodeCoreType(getExn(value, "typ")))
  | "pattern" =>
    Ast.PPat({
      pat: decodePattern(getExn(value, "pat")),
      guard: decodeOption(decodeExpression, get(value, "guard")),
    })
  | other => raiseDecodeError("Unsupported payload kind: " ++ other)
  }

let decodeOptionalPayload = (value: option<raw>): option<Ast.payload> =>
  decodeOption(decodePayload, value)

let decodeTypeDeclarations = (value: raw): array<Ast.typeDeclaration> =>
  value->rawToArray->Belt.Array.map(decodeTypeDeclaration)

let obj = (): dict<raw> => Dict.make()

let put = (dict: dict<raw>, key: string, value: raw) => {
  Dict.set(dict, key, value)
  dict
}

let putOpt = (dict: dict<raw>, key: string, value: option<raw>) => {
  switch value {
  | None => ()
  | Some(value) => Dict.set(dict, key, value)
  }
  dict
}

let encodeLocation = (value: Ast.location): raw => {
  let dict = obj()
  dict->putOpt("file", value.file->Belt.Option.map(stringToRaw))->ignore
  dict->put("startLine", intToRaw(value.startLine))->ignore
  dict->put("startCol", intToRaw(value.startCol))->ignore
  dict->put("endLine", intToRaw(value.endLine))->ignore
  dict->put("endCol", intToRaw(value.endCol))->ignore
  dict->put("ghost", boolToRaw(value.ghost))->ignore
  dictToRaw(dict)
}

let encodeArgLabel = (value: Ast.argLabel): raw => {
  let dict = obj()
  switch value {
  | Ast.Nolabel => dict->put("kind", stringToRaw("nolabel"))->ignore
  | Ast.Labelled(name) =>
    dict->put("kind", stringToRaw("labelled"))->ignore
    dict->put("name", stringToRaw(name))->ignore
  | Ast.Optional(name) =>
    dict->put("kind", stringToRaw("optional"))->ignore
    dict->put("name", stringToRaw(name))->ignore
  }
  dictToRaw(dict)
}

let encodeConstant = (value: Ast.constant): raw => {
  let dict = obj()
  switch value {
  | Ast.Pconst_integer({value, suffix}) =>
    dict->put("kind", stringToRaw("integer"))->ignore
    dict->put("value", stringToRaw(value))->ignore
    dict->putOpt("suffix", suffix->Belt.Option.map(stringToRaw))->ignore
  | Ast.Pconst_char({value}) =>
    dict->put("kind", stringToRaw("char"))->ignore
    dict->put("value", intToRaw(value))->ignore
  | Ast.Pconst_string({value, delimiter}) =>
    dict->put("kind", stringToRaw("string"))->ignore
    dict->put("value", stringToRaw(value))->ignore
    dict->putOpt("delimiter", delimiter->Belt.Option.map(stringToRaw))->ignore
  | Ast.Pconst_float({value, suffix}) =>
    dict->put("kind", stringToRaw("float"))->ignore
    dict->put("value", stringToRaw(value))->ignore
    dict->putOpt("suffix", suffix->Belt.Option.map(stringToRaw))->ignore
  }
  dictToRaw(dict)
}

let rec encodeExpression = (value: Ast.expression): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  let desc = obj()
  switch value.desc {
  | Ast.Pexp_ident({path}) =>
    desc->put("kind", stringToRaw("ident"))->ignore
    desc->put("path", arrayToRaw(path->Belt.Array.map(stringToRaw)))->ignore
  | Ast.Pexp_constant({constant}) =>
    desc->put("kind", stringToRaw("constant"))->ignore
    desc->put("constant", encodeConstant(constant))->ignore
  | Ast.Pexp_apply({funct, args}) =>
    desc->put("kind", stringToRaw("apply"))->ignore
    desc->put("funct", encodeExpression(funct))->ignore
    desc
    ->put(
      "args",
      arrayToRaw(
        args->Belt.Array.map(arg => {
          let argDict = obj()
          argDict->put("label", encodeArgLabel(arg.label))->ignore
          argDict->put("expr", encodeExpression(arg.argExpr))->ignore
          dictToRaw(argDict)
        }),
      ),
    )
    ->ignore
  | Ast.Pexp_fun({argLabel, default, lhs, rhs, async}) =>
    desc->put("kind", stringToRaw("fun"))->ignore
    desc->put("argLabel", encodeArgLabel(argLabel))->ignore
    desc->putOpt("default", default->Belt.Option.map(encodeExpression))->ignore
    desc->put("lhs", encodePattern(lhs))->ignore
    desc->put("rhs", encodeExpression(rhs))->ignore
    desc->put("async", boolToRaw(async))->ignore
  | Ast.Pexp_construct({constructPath, constructArg}) =>
    desc->put("kind", stringToRaw("construct"))->ignore
    desc->put("path", arrayToRaw(constructPath->Belt.Array.map(stringToRaw)))->ignore
    desc->putOpt("arg", constructArg->Belt.Option.map(encodeExpression))->ignore
  | Ast.Pexp_let({recursive, bindings, body}) =>
    desc->put("kind", stringToRaw("let"))->ignore
    desc->put("recursive", boolToRaw(recursive))->ignore
    desc->put("bindings", arrayToRaw(bindings->Belt.Array.map(encodeValueBinding)))->ignore
    desc->put("body", encodeExpression(body))->ignore
  | Ast.Pexp_sequence({first, second}) =>
    desc->put("kind", stringToRaw("sequence"))->ignore
    desc->put("first", encodeExpression(first))->ignore
    desc->put("second", encodeExpression(second))->ignore
  | Ast.Pexp_record({fields, withExpr}) =>
    desc->put("kind", stringToRaw("record"))->ignore
    desc
    ->put(
      "fields",
      arrayToRaw(
        fields->Belt.Array.map(field => {
          let fieldDict = obj()
          fieldDict->put("lid", arrayToRaw(field.lid->Belt.Array.map(stringToRaw)))->ignore
          fieldDict->put("expr", encodeExpression(field.expr))->ignore
          fieldDict->put("optional", boolToRaw(field.optional))->ignore
          dictToRaw(fieldDict)
        }),
      ),
    )
    ->ignore
    desc->putOpt("withExpr", withExpr->Belt.Option.map(encodeExpression))->ignore
  | Ast.Pexp_field({exprTarget, path}) =>
    desc->put("kind", stringToRaw("field"))->ignore
    desc->put("expr", encodeExpression(exprTarget))->ignore
    desc->put("path", arrayToRaw(path->Belt.Array.map(stringToRaw)))->ignore
  }
  dict->put("desc", dictToRaw(desc))->ignore
  dictToRaw(dict)
}
and encodePattern = (value: Ast.pattern): raw => {
  let dict = obj()
  value.patternLoc
  ->Belt.Option.map(encodeLocation)
  ->Belt.Option.map(loc => dict->put("loc", loc))
  ->ignore
  let desc = obj()
  switch value.patternDesc {
  | Ast.Ppat_any => desc->put("kind", stringToRaw("any"))->ignore
  | Ast.Ppat_var({name}) =>
    desc->put("kind", stringToRaw("var"))->ignore
    desc->put("name", stringToRaw(name))->ignore
  | Ast.Ppat_record({patternFields, closed}) =>
    desc->put("kind", stringToRaw("record"))->ignore
    desc
    ->put(
      "fields",
      arrayToRaw(
        patternFields->Belt.Array.map(field => {
          let fieldDict = obj()
          fieldDict->put("lid", arrayToRaw(field.fieldLid->Belt.Array.map(stringToRaw)))->ignore
          fieldDict->put("pat", encodePattern(field.pat))->ignore
          fieldDict->put("optional", boolToRaw(field.patternOptional))->ignore
          dictToRaw(fieldDict)
        }),
      ),
    )
    ->ignore
    desc->put("closed", boolToRaw(closed))->ignore
  }
  dict->put("desc", dictToRaw(desc))->ignore
  dictToRaw(dict)
}
and encodeCoreType = (value: Ast.coreType): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  let desc = obj()
  switch value.desc {
  | Ast.Ptyp_constr({path, args}) =>
    desc->put("kind", stringToRaw("constr"))->ignore
    desc->put("path", arrayToRaw(path->Belt.Array.map(stringToRaw)))->ignore
    desc->put("args", arrayToRaw(args->Belt.Array.map(encodeCoreType)))->ignore
  | Ast.Ptyp_arrow({argLabel, arg, ret}) =>
    desc->put("kind", stringToRaw("arrow"))->ignore
    desc->put("argLabel", encodeArgLabel(argLabel))->ignore
    desc->put("arg", encodeCoreType(arg))->ignore
    desc->put("ret", encodeCoreType(ret))->ignore
  | Ast.Ptyp_tuple({items}) =>
    desc->put("kind", stringToRaw("tuple"))->ignore
    desc->put("items", arrayToRaw(items->Belt.Array.map(encodeCoreType)))->ignore
  }
  dict->put("desc", dictToRaw(desc))->ignore
  dictToRaw(dict)
}
and encodeValueBinding = (value: Ast.valueBinding): raw => {
  let dict = obj()
  value.bindingLoc
  ->Belt.Option.map(encodeLocation)
  ->Belt.Option.map(loc => dict->put("loc", loc))
  ->ignore
  dict->put("pat", encodePattern(value.bindingPat))->ignore
  dict->put("expr", encodeExpression(value.bindingExpr))->ignore
  dictToRaw(dict)
}
and encodeValueDescription = (value: Ast.valueDescription): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  dict->put("name", stringToRaw(value.name))->ignore
  dict->put("typ", encodeCoreType(value.typ))->ignore
  dictToRaw(dict)
}
and encodeLabelDeclaration = (value: Ast.labelDeclaration): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  dict->put("name", stringToRaw(value.name))->ignore
  dict->put("mutable", boolToRaw(value.mutable_))->ignore
  dict->put("optional", boolToRaw(value.optional))->ignore
  dict->put("typ", encodeCoreType(value.typ))->ignore
  dictToRaw(dict)
}
and encodeConstructorDeclaration = (value: Ast.constructorDeclaration): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  dict->put("name", stringToRaw(value.name))->ignore
  dict->put("args", arrayToRaw(value.args->Belt.Array.map(encodeCoreType)))->ignore
  dictToRaw(dict)
}
and encodeTypeKind = (value: Ast.typeKind): raw => {
  let dict = obj()
  switch value {
  | Ast.Ptype_abstract => dict->put("kind", stringToRaw("abstract"))->ignore
  | Ast.Ptype_record({fields}) =>
    dict->put("kind", stringToRaw("record"))->ignore
    dict->put("fields", arrayToRaw(fields->Belt.Array.map(encodeLabelDeclaration)))->ignore
  | Ast.Ptype_variant({constructors}) =>
    dict->put("kind", stringToRaw("variant"))->ignore
    dict
    ->put("constructors", arrayToRaw(constructors->Belt.Array.map(encodeConstructorDeclaration)))
    ->ignore
  }
  dictToRaw(dict)
}
and encodeTypeDeclaration = (value: Ast.typeDeclaration): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  dict->put("name", stringToRaw(value.name))->ignore
  dict->put("params", arrayToRaw(value.params->Belt.Array.map(encodeCoreType)))->ignore
  dict->putOpt("manifest", value.manifest->Belt.Option.map(encodeCoreType))->ignore
  dict->put("kind", encodeTypeKind(value.kind))->ignore
  dictToRaw(dict)
}
and encodeStructureItem = (value: Ast.structureItem): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  let desc = obj()
  switch value.desc {
  | Ast.Pstr_eval({expr}) =>
    desc->put("kind", stringToRaw("eval"))->ignore
    desc->put("expr", encodeExpression(expr))->ignore
  | Ast.Pstr_value({recursive, bindings}) =>
    desc->put("kind", stringToRaw("value"))->ignore
    desc->put("recursive", boolToRaw(recursive))->ignore
    desc->put("bindings", arrayToRaw(bindings->Belt.Array.map(encodeValueBinding)))->ignore
  | Ast.Pstr_type({recursive, declarations}) =>
    desc->put("kind", stringToRaw("type"))->ignore
    desc->put("recursive", boolToRaw(recursive))->ignore
    desc
    ->put("declarations", arrayToRaw(declarations->Belt.Array.map(encodeTypeDeclaration)))
    ->ignore
  }
  dict->put("desc", dictToRaw(desc))->ignore
  dictToRaw(dict)
}
and encodeSignatureItem = (value: Ast.signatureItem): raw => {
  let dict = obj()
  value.loc->Belt.Option.map(encodeLocation)->Belt.Option.map(loc => dict->put("loc", loc))->ignore
  let desc = obj()
  switch value.desc {
  | Ast.Psig_value({value}) =>
    desc->put("kind", stringToRaw("value"))->ignore
    desc->put("value", encodeValueDescription(value))->ignore
  | Ast.Psig_type({recursive, declarations}) =>
    desc->put("kind", stringToRaw("type"))->ignore
    desc->put("recursive", boolToRaw(recursive))->ignore
    desc
    ->put("declarations", arrayToRaw(declarations->Belt.Array.map(encodeTypeDeclaration)))
    ->ignore
  }
  dict->put("desc", dictToRaw(desc))->ignore
  dictToRaw(dict)
}

let encodeExtensionResult = (value: Extension.result): raw => {
  let dict = obj()
  switch value {
  | Extension.Pexp(expr) =>
    dict->put("kind", stringToRaw("expression"))->ignore
    dict->put("expr", encodeExpression(expr))->ignore
  | Extension.Pstr(items) =>
    dict->put("kind", stringToRaw("structure"))->ignore
    dict->put("items", arrayToRaw(items->Belt.Array.map(encodeStructureItem)))->ignore
  | Extension.Psig(items) =>
    dict->put("kind", stringToRaw("signature"))->ignore
    dict->put("items", arrayToRaw(items->Belt.Array.map(encodeSignatureItem)))->ignore
  }
  dictToRaw(dict)
}

let encodeDeriverResult = (value: Deriver.result): raw => {
  let dict = obj()
  dict->put("structure", arrayToRaw(value.structure->Belt.Array.map(encodeStructureItem)))->ignore
  dict->put("signature", arrayToRaw(value.signature->Belt.Array.map(encodeSignatureItem)))->ignore
  dictToRaw(dict)
}
